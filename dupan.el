;;; dupan.el --- 百度网盘客户端 -*- lexical-binding: t -*-

;; Copyright (C) 2022 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/emacs-baidupan
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:

;; Emacs 上的百度网盘客户端。
;;
;;  可以通过 Dired 方式对网盘文件进行最基本的增删查、移动、拷贝、上传、下载。
;;  能够使用类似 Tramp 的方式对文件进行基本修改。
;;
;; 基本使用步骤:
;;
;;  1. 下载本文件，并加载之 (load or require)
;;  2. 确保拥有百度帐号后，执行 `M-x dupan-add-account` 命令生成记录身份信息的配置文件
;;  3. 之后，使用 `M-x dupan-find` 搜索网盘文件并打开。
;;     亦或使用 `C-x C-f /dp:/网盘文件路径` 直接打开指定文件。
;;  4. 你可以继续执行 `M-x dupan-add-account` 添加另外的帐号，并使用 `M-x dupan-switch-account` 进行切换。
;;

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'url-http)
(require 'dired-aux)

(eval-when-compile
  (defvar url-http-end-of-headers)
  (defvar url-http-content-type)
  (defvar url-http-response-status))

(defgroup dupan nil
  "百度网盘客户端。"
  :prefix "dupan-"
  :group 'applications)

(defcustom dupan-debug nil
  "用来控制调试信息的输出。"
  :type 'boolean)

(defcustom dupan-config-file (locate-user-emacs-file ".pan-baidu-com")
  "用于保存身份信息的配置文件。"
  :type 'string)

(defcustom dupan-sleep-ticks 0.3
  "无聊的参数。为了避免过多的 '请求过于频繁'，让某些 API 在请求中有这些时间的等待时间。"
  :type 'number)

(defvar dupan-prefix "/dp:")

(defvar dupan--config nil)

(defconst dupan--urls
  `((auth        . "https://openapi.baidu.com/oauth/2.0/authorize")
    (token       . "https://openapi.baidu.com/oauth/2.0/token")
    (quota       . "https://pan.baidu.com/api/quota")
    (nas         . "https://pan.baidu.com/rest/2.0/xpan/nas")
    (file        . "https://pan.baidu.com/rest/2.0/xpan/file")
    (multimedia  . "https://pan.baidu.com/rest/2.0/xpan/multimedia")
    (superfile   . "https://d.pcs.baidu.com/rest/2.0/pcs/superfile2")))

(defconst dupan--errors
  `((-6    . "身份验证失败")
    (-7    . "文件或目录名错误或无权访问")
    (-8    . "文件或目录已存在")
    (-9    . "文件或目录不存在")
    (-10   . "云端容量已满")
    (-55   . "访问太频繁，已触发访问控制")
    (2     . "参数错误")
    (10    . "创建文件失败")
    (111   . "Token 错误")
    (31024 . "申请开通上传权限")
    (31034 . "请求过于频繁，稍后再试")
    (31190 . "文件不存在")
    (31299 . "第一个分片的大小小于4MB")
    (31326 . "命中防盗链，需检查 User-Agent 请求头是否正常")
    (31362 . "签名错误，请检查链接地址是否完整")
    (31363 . "分片缺失")
    (31364 . "超出分片大小限制")
    (42214 . "文件基础信息查询失败"))
  "这些错误码是根据文档并在尝试中猜的，不一定都对。不知道有没有具体的文档说明。")

(defvar-local dupan--sort-type nil
  "Dired 中文件列表的排序方式，为 `dupan--sorts' 中的一项。")

(defconst dupan--sorts
  '(("name" . nil)
    ("name" . t)
    ("time" . t)
    ("time" . nil)
    ("size" . t)
    ("size" . nil))
  "按啥排序。(排序依据 . 是否逆序)")

(defvar dupan-make-download-cmdline-function #'dupan-make-curl-download-cmdline)


;;; Utility

(defun dupan-make-url (url &rest args)
  (declare (indent 1))
  (let* ((url (if (stringp url) url (alist-get url dupan--urls)))
         (params (cl-loop for (k v) on args by #'cddr
                          collect (format "%s=%s" (substring (symbol-name k) 1) v) into fs
                          finally return (string-join fs "&"))))
    (unless (string-match-p "/oauth" url)
      (setq params (concat params "&access_token=" (dupan-get-token))))
    (concat url (if (string-match-p "?" url) "&" "?") params)))

(cl-defun dupan-make-request (url &key method data headers callback)
  (when (consp data)
    (cl-loop for (k . v) in data
             collect (format "%s=%s" k v) into ds
             finally (setq data (string-join ds "&"))))
  (let ((url-request-data data)
        (url-request-method (or method (if data "POST" "GET")))
        (url-user-agent (unless (alist-get "Content-Type" headers) "pan.baidu.com"))
        (url-request-extra-headers headers)
        (parse-resp (lambda ()
                      (unwind-protect
                          (cond ((not (< 199 url-http-response-status 300))
                                 (user-error "有错误发生了 (%s)  %s" url-http-response-status
                                             (string-trim (buffer-substring url-http-end-of-headers (point-max)))))
                                ((and (not (string-match-p "method=upload" url)) ; 坑爹，返回的是 json, content-type 却设置为 html...
                                      (string-prefix-p "text/html" url-http-content-type))
                                 (user-error "有错误发生了，返回了一坨 html 内容: %s" (buffer-string)))
                                (t (dupan-info "Parsing response..")
                                   (set-buffer-multibyte t)
                                   (goto-char url-http-end-of-headers)
                                   (save-excursion
                                     (decode-coding-region (point) (point-max) 'utf-8))
                                   (let ((res (json-read)))
                                     (dupan-info "RESPONSE: %s" res)
                                     (dupan-ensure-results res))))
                        (kill-buffer)))))
    (dupan-info "[%s]: %s, data: %s" url-request-method url
                (if (string-match-p "method=upload" url) "[octet form-data]" url-request-data))
    (if callback ; async | sync
        (url-retrieve url (lambda (_status buf)
                            (let ((r (funcall parse-resp)))
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (funcall callback r)))))
                      (list (current-buffer)) t)
      (with-current-buffer (url-retrieve-synchronously url t)
        (funcall parse-resp)))))

(defun dupan-ensure-results (json)
  (let ((errno (alist-get 'errno json)))
    (if (or (null errno) (equal errno 0)) json
      (user-error
       (if-let ((errmsg (alist-get errno dupan--errors)))
           (format "[%s] %s" errno errmsg)
         (let ((msg (alist-get 'errmsg json)))
           (concat (format "未知错误: %s" errno)
                   (if msg (concat " (" msg ")") ""))))))))

(defun dupan-make-multipart-body (data to)
  "生成 form-data。
使用 `mm-url-encode-multipart-form-data' 生成的 form-data 总报
maybe request body not standard 的错误。莫名其妙，干脆自己拼得了。"
  (let* ((boundary "--------------------------94c6bfb89dd7d006")
         (body (concat "--" boundary "--\r\n"
                       "Content-Disposition: form-data; name=\"file\"; filename=\"" (url-encode-url (url-file-nondirectory to)) "\"\r\n"
                       "Content-Type: application/octet-stream\r\n\r\n"
                       data "\r\n"
                       "--" boundary "--")))
    (cons body boundary)))

(defun dupan-info (fmt-string &rest args)
  (when dupan-debug
    (apply #'message (concat "\t[百度网盘] " fmt-string) args)))

(defun dupan-normalize (path &optional force-prefix-p)
  (when (and path (string-match-p (concat "^" dupan-prefix) path))
    (setq path (substring path (length dupan-prefix))))
  (when (= 0 (length path))
    (setq path "/"))
  (when (string-match-p "./$" path)
    (setq path (substring path nil -1)))
  (unless (string-match-p "^/" path)
    (setq path (concat "/" path)))
  (concat (if force-prefix-p dupan-prefix) path))

(defun dupan-file-p (filename)
  (string-prefix-p dupan-prefix filename))

(defun dupan-file-size (file)
  (file-attribute-size (file-attributes file)))

(defun dupan-file-md5sum (file &optional beg end)
  "获取 FILE 的 md5 值。如果指定了 BEG 和 END，那么只求取这个区间内容的 MD5。
可以借助外部的 md5sum 进行优化。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil beg end)
    (md5 (current-buffer))))

(defvar dupan-slice-size (* 4 1024 1024) "上传文件分片的尺寸。按要求 4M 大小。")

(defun dupan-file-slice-points (filename)
  "按照 `dupan-slice-size' 对文件进行分片处理，返回分片的点位。"
  (cl-assert (file-regular-p filename))
  (cl-loop with fs = (dupan-file-size filename)
           for i from 0
           until (>= (* i dupan-slice-size) fs)
           collect (cons (* i dupan-slice-size)
                         (min (* (+ i 1) dupan-slice-size) fs))))

(defun dupan-make-curl-download-cmdline (dlink meta)
  "生成使用 curl 进行下载的命令行语句。DLINK 为下载地址，META 包含下载文件的基本信息。"
  (let ((name (alist-get 'server_filename meta)))
    (format "curl -L '%s' -H 'User-Agent: pan.baidu.com' -o '%s'" dlink name)))


;;; Cache

(defvar-local dupan--local-files nil "用于 Dired buffer 中缓存文件夹下所有文件的信息。")

(defun dupan--find-with-cache (filename &optional renew)
  (let (ret)
    (unless renew
      (setq ret (cl-find-if (lambda (f) (string= (alist-get 'path f) filename)) dupan--local-files)))
    (when (or renew (null ret))
      (setq dupan--local-files
            (cl-remove-if (lambda (f) (string= (alist-get 'path f) filename)) dupan--local-files))
      (when-let ((file (dupan-req 'finfo filename)))
        (push file dupan--local-files)
        (setq ret file)))
    ret))

(defvar dupan--ttl-cache nil "全局缓存，可保持数据存活若干时间。主要用来缓解 31034 号错误太多的问题。")
(defvar dupan--ttl-time 8 "默认存活 8 秒")

(defun dupan-set-ttl-cache (key value)
  (setq dupan--ttl-cache (assoc-delete-all key dupan--ttl-cache))
  (when value
    (push (cons key (cons value (current-time))) dupan--ttl-cache)))

(defmacro dupan-with-ttl-cache (key &rest body)
  "优先取缓存。"
  (declare (indent 1))
  `(let* ((key ,key)
          (cache (cdr (assoc-string key dupan--ttl-cache))))
     (if (and cache (< (time-to-seconds (time-since (cdr cache))) dupan--ttl-time))
         (progn (dupan-info "[命中缓存] %s: %s..." key (car cache)) (car cache))
       (prog1 (setq cache (progn ,@body))
         (dupan-set-ttl-cache key cache)))))


;;; Authorization

(defun dupan-dump-to-file (list file)
  "Pretty print LIST to FILE."
  (with-temp-file file
    (insert ";;-*- mode: emacs-lisp -*-\n")
    (pp list (current-buffer))))

;;;###autoload
(defun dupan-add-account ()
  "用来生成包含身份验证信息的配置文件。"
  (interactive)
  (if (and (file-exists-p dupan-config-file)
           (not (y-or-n-p (format "配置文件 %s 已存在，并存在帐号 %s，继续将添加新的帐号。是否继续？"
                                  dupan-config-file
                                  (progn
                                    (dupan-load-config)
                                    (mapconcat (lambda (acc) (alist-get 'name acc)) dupan--config "、"))))))
      (user-error "操作已取消")
    (let ((newp (null dupan--config))
          (help "
要生成配置文件，需要你提供一些信息。如果你是第一次使用百度开放平台，按下面步骤操作：\n
1、请先在网页中打开 https://pan.baidu.com/union/apply 页面，登录帐号，并申请接入（个人）
2、填写信息，完成身份认证的审核
3、点击进入控制台页面，创建一个应用（个人最多只能创建两个应用，且创建后无法删除，所以创建前请仔细阅读文档），
   然后你将获取到此应用相关的 AppKey/SecretKey 等
4、进入安全设置，将回调页设置为一个有效 URL，比如 https://pan.baidu.com/union/console

详情阅读百度开放平台的相关文档。你是否已经搞掂且要继续？"))
      (when (y-or-n-p help)
        (let ((id (read-string "请输入您的 [AppKey]: ")))
          (when-let ((acc (cl-find-if (lambda (a) (string= (alist-get 'id a) id)) dupan--config)))
            (user-error "要添加的帐号已存在，请确认是否搞错！ %s : %s" (alist-get 'name acc) id))
          (let ((secret   (read-string "请输入您的 [SecretKey]: "))
                (redirect (read-string "请输入您的 [回调页面]: ")))
            (when (y-or-n-p (format "您输入的信息为:\n\n AppKey:\t%s\n SecretKey:\t%s\n 回调页面:\t%s\n\n是否确认？" id secret redirect))
              (let ((url (dupan-make-url 'auth :response_type "code" :scope "basic,netdisk" :client_id id :redirect_uri redirect)))
                (browse-url url)
                (let* ((code (read-string "接下来到浏览器中在弹出的页面中进行授权，将返回页面 url 中的 code 填到此处: "))
                       (tokens (dupan-get-refresh-token code id secret redirect))
                       (account `((id       . ,id)
                                  (secret   . ,secret)
                                  (redirect . ,redirect)
                                  (refresh-token . ,(alist-get 'refresh_token tokens))
                                  (access-token  . ,(alist-get 'access_token tokens))
                                  (expires-in   .  ,(time-add (current-time) (seconds-to-time (- (alist-get 'expires_in tokens) 120))))))
                       (name (let ((dupan--config (list account)))
                               (alist-get 'baidu_name (dupan-req 'uinfo)))))
                  (setf (alist-get 'name account) name)
                  (setq dupan--config (append dupan--config (list account)))
                  (dupan-dump-to-file dupan--config dupan-config-file)
                  (if newp
                      (message "配置文件 %s 生成成功!" dupan-config-file)
                    (message "帐号 %s 添加成功。可以使用 'dupan-switch-account' 切换帐号。" name)))))))))))

;;;###autoload
(defun dupan-switch-account ()
  "用来切换到其他帐号。只在添加了多个帐号的情况下有用。"
  (interactive)
  (unless (dupan-load-config)
    (user-error "没发现任何的帐号信息，请确认配置文件已经加载"))
  (let ((cs (mapcar (lambda (acc) (alist-get 'name acc))
                    (cdr dupan--config))))
    (unless cs (user-error "当前只添加了一个帐号，没必要切换。"))
    (let* ((name (completing-read "选择要切换的帐号: " cs nil t))
           (newcf (cl-loop for acc in dupan--config
                           if (string-equal name (alist-get 'name acc)) collect acc into first
                           else collect acc into others
                           finally return (append first others))))
      (dupan-dump-to-file newcf dupan-config-file)
      (dupan-load-config))))

(defun dupan-load-config ()
  (if (file-exists-p dupan-config-file)
      (let (accs)
        (ignore-errors
          (let ((rs (with-temp-buffer
                      (insert-file-contents dupan-config-file)
                      (read (current-buffer)))))
            (setq accs (if (consp (caar rs)) rs (list rs)))))
        (unless (alist-get 'refresh-token (car accs))
          (user-error "配置文件 %s 存在错误。" dupan-config-file))
        (setq dupan--config accs))
    (user-error "没找到配置文件 '%s', 请先调用 'M-x dupan-add-account' 生成一个" dupan-config-file)))

(defun dupan-get-config (&optional force)
  (or (unless force dupan--config)
      (setq dupan--config (dupan-load-config))))

(defun dupan-get-refresh-token (code id secret redirect)
  (let ((url (dupan-make-url 'token
               :grant_type "authorization_code" :code code :client_id id :client_secret secret :redirect_uri redirect)))
    (dupan-make-request url)))

(defun dupan-get-token ()
  (let* ((config (car (dupan-get-config)))
         (expired (alist-get 'expires-in config))
         (token (alist-get 'access-token config))
         (avail (and token expired (time-less-p (current-time) expired))))
    (cond
     (avail token)
     ((and token expired (not avail))
      (message "[百度网盘] 刷新 token...")
      (let* ((url (dupan-make-url 'token
                    :grant_type "refresh_token"
                    :refresh_token (alist-get 'refresh-token config)
                    :client_id     (alist-get 'id config)
                    :client_secret (alist-get 'secret config)))
             (result (dupan-make-request url)))
        (if result
            (let* ((rk (alist-get 'refresh_token result))
                   (tk (alist-get 'access_token result))
                   (etime (time-add (current-time) (seconds-to-time (- (alist-get 'expires_in result) 120)))))
              (setf (alist-get 'refresh-token (car dupan--config)) rk)
              (setf (alist-get 'access-token (car dupan--config)) tk)
              (setf (alist-get 'expires-in (car dupan--config)) etime)
              (dupan-dump-to-file dupan--config dupan-config-file))
          (user-error "在请求 refresh_token 的时候有错误发生?"))))
     (t (user-error "配置文件有误")))))


;;; API

(cl-defgeneric dupan-req (api-key &rest path-callback-and-others))

(cl-defmethod dupan-req :around (func &rest args)
  (dupan-info "正请求 %s %s..." func args)
  (dupan-get-token)
  (cond ((eql func 'finfo)
         (dupan-with-ttl-cache (concat "finfo:" (car args))
           (cl-call-next-method)))
        (t (apply #'cl-call-next-method func args))))

(cl-defmethod dupan-req ((_ (eql 'uinfo)))
  (dupan-make-request (dupan-make-url 'nas :method 'uinfo)))

(cl-defmethod dupan-req ((_ (eql 'quota)))
  (dupan-make-request (dupan-make-url 'quota :checkfree 1 :checkexipre 1)))

(cl-defmethod dupan-req ((_ (eql 'list)) dir &optional callback)
  "当带回调函数的时候，使用异步的方式进行请求，否则同步。"
  (let* ((ord (or dupan--sort-type (car dupan--sorts)))
         (url (dupan-make-url 'file
                :method 'list :dir (url-hexify-string (or dir "/"))
                :start 0 :limit 1000 :web 0 :folder 0 :order (car ord))))
    (when (cdr ord)
      (setq url (concat url "&desc=1")))
    (if callback
        (dupan-make-request url :callback
                            (lambda (res)
                              (funcall callback
                                       (cl-coerce (alist-get 'list res) 'list))))
      (cl-coerce (alist-get 'list (dupan-make-request url)) 'list))))

(cl-defmethod dupan-req ((_ (eql 'search)) text &optional dir norecur)
  (let* ((url (dupan-make-url 'file
                :method 'search :key (url-hexify-string text) :dir (url-hexify-string (or dir "/"))
                :web 0 :recursion (if norecur 0 1) :num 500))
         (res (dupan-make-request url)))
    (cl-coerce (alist-get 'list res) 'list)))

(cl-defmethod dupan-req ((_ (eql 'meta)) fsids)
  (let* ((fsids (if (atom fsids) (list fsids) fsids))
         (fsids (json-encode fsids))
         (url (dupan-make-url 'multimedia
                :method 'filemetas :fsids fsids :web 0 :thumb 1 :extra 1 :dlink 1))
         (rs (dupan-make-request url)))
    (cl-coerce (alist-get 'list rs) 'list)))

(cl-defmethod dupan-req ((_ (eql 'finfo)) path &optional dlink?)
  "根据路径获取文件的基本信息。如果 DLINK? 为真，那么将额外获取下载地址。"
  (if (or (string= "" path) (string= "/" path))
      '((isdir . 1))
    (let* ((dir (file-name-directory path))
           (name (file-name-nondirectory path))
           (rs (dupan-req 'search (substring name nil (min (length name) 18)) dir t))
           (file (cl-find-if (lambda (f) (string= (alist-get 'path f) path)) rs)))
      (when (and dlink? file (equal (alist-get 'isdir file) 0))
        (when-let ((meta (dupan-req 'meta (alist-get 'fs_id file))))
          (setf (alist-get 'dlink file) (alist-get 'dlink (car meta)))))
      file)))

(cl-defmethod dupan-req ((_ (eql 'mkdir)) path)
  (let* ((url (dupan-make-url 'file :method 'create))
         (data `((path . ,(url-hexify-string path))
                 (isdir . 1) (size . 0) (rtype . 0))))
    (dupan-make-request url :data data)))

(cl-defmethod dupan-req ((_ (eql 'delete)) file)
  (let* ((files (json-encode `(((path . ,(url-hexify-string file))))))
         (url (dupan-make-url 'file :method 'filemanager :opera 'delete))
         (data `((filelist . ,files) (ondup . fail))))
    ;; 即使删除失败，也不会有错误码。闹那般啊！
    (alist-get 'info (dupan-make-request url :data data))))

(defun dupan--copy-or-move (type from to)
  "在网盘中进行文件的复制或移动。通常 FROM 和 TO 表示的从哪个文件复制或移动为哪个文件。
如果 FROM 是个列表且 TO 是个文件夹，那么是将 FROM 中所有文件复制或移动到 TO 文件夹的意思。"
  (cl-assert (member type '(copy move)))
  (let* ((single (atom from))
         (dest (if single (file-name-directory to) to))
         (data `((async . 0)
                 (filelist . ,(json-encode
                               (cl-loop
                                for f in (if single (list from) from)
                                collect `((path . ,(url-hexify-string f))
                                          (dest . ,(url-hexify-string dest))
                                          (newname . ,(url-hexify-string
                                                       (file-name-nondirectory (if single to f))))))))))
         (url (dupan-make-url 'file :method 'filemanager :opera type)))
    (dupan-make-request url :data data)))

(cl-defmethod dupan-req ((_ (eql 'copy)) from to)
  (dupan--copy-or-move 'copy from to))

(cl-defmethod dupan-req ((_ (eql 'move)) from to)
  (dupan--copy-or-move 'move from to))

(defun dupan--upload-precreate (from to &optional overridep)
  "进行预上传。"
  (unless (file-exists-p from)
    (user-error "没找到你要上传的文件 %s 啊" from))
  (let* ((size (dupan-file-size from))
         (url (dupan-make-url 'file :method 'precreate))
         (md5 (dupan-file-md5sum from))
         ;; 分片，计算
         (slice-points (dupan-file-slice-points from))
         ;; 分片，MD5
         (block-list (or (cl-loop for (beg . end) in slice-points
                                  collect (dupan-file-md5sum from beg end))
                         (list md5)))
         ;; 发送预上传请求
         (data `((path . ,(url-hexify-string to))
                 (rtype . ,(if overridep 3 0))
                 (size . ,size)
                 (isdir . 0)
                 (autoinit . 1)
                 (block_list . ,(json-encode block-list))
                 (content-md5 . ,md5)))
         (rs (dupan-make-request url :data data)))
    (list :uploadid (alist-get 'uploadid rs)
          :return-type (alist-get 'return_type rs)
          :slice-points slice-points
          :block-list block-list
          :md5 md5 :size size)))

(defun dupan--upload-contents (from to meta)
  "META 是预上传阶段返回的结果，包括 uploadid 和分片结果等。"
  (cl-loop with uploadid = (plist-get meta :uploadid)
           with ps = (plist-get meta :slice-points)
           for i from 0
           for p in ps
           for url = (dupan-make-url 'superfile
                       :method 'upload
                       :type 'tmpfile
                       :path (url-hexify-string to)
                       :uploadid uploadid
                       :partseq i)
           for raw = (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally from nil (car p) (cdr p))
                       (buffer-string))
           for data = (dupan-make-multipart-body raw to)
           for headers = `(("Content-Type" . ,(concat "multipart/form-data; boundary=" (cdr data))))
           collect (progn (when (> (length ps) 1)
                            (message "正在上传第 %s/%s 个分片..." (+ i 1) (length ps)))
                          (dupan-make-request url :data (car data) :headers headers))))

(cl-defmethod dupan-req ((_ (eql 'upload)) from to &optional overridep)
  ;; 预上传
  (let ((meta (dupan--upload-precreate from to overridep)))
    (unless (equal (plist-get meta :return-type) 2) ; 如果是 2 说明已经上传完毕，将没必要继续了
      ;; 分片上传
      (dupan--upload-contents from to meta)
      ;; 完成上传
      (let* ((data `((path . ,(url-hexify-string to))
                     (uploadid . ,(plist-get meta :uploadid))
                     (block_list . ,(json-encode (plist-get meta :block-list)))
                     (size . ,(plist-get meta :size))
                     (rtype . ,(if overridep 3 0))
                     (isdir . 0)))
             (url (dupan-make-url 'file :method 'create)))
        (dupan-make-request url :data data)))))

(cl-defmethod dupan-req ((_ (eql 'download)) dlink to)
  (with-temp-message nil
    (let ((url (dupan-make-url dlink)))
      (url-copy-file url to))))

(cl-defmethod dupan-req ((_ (eql 'streaming)) path &optional ad-token)
  "用来获取视频播放 M3U8 文件。TODO: 需要完善。"
  (let* ((url (dupan-make-url 'file
                :method 'streaming :path (url-hexify-string path)
                :type "M3U8_AUTO_1080" :addToken ad-token))
         (headers `(("Content-Type" . "xpanvideo;$appName;$appVersion;$sysName;$sysVersion;ts"))))
    (dupan-make-request url :headers headers)))

;; (dupan-req 'uinfo)
;; (dupan-req 'quota)
;; (dupan-req 'list "/apps")
;; (dupan-req 'search "zip")
;; (dupan-req 'search "三国演义.zip")
;; (dupan-req 'delete "/apps/vvv")
;; (dupan-req 'copy "/xxx/aaa" "/yyy/bbb")
;; (dupan-req 'upload "~/.bashrc" "/apps/vvv/xxx.js" t)
;; (dupan-req 'mkdir "/vvv")
;; (dupan-req 'delete "/vvv")
;; (dupan-req 'download "" "~/vvv/ddd.lisp")

(defun dupan--ignore (&optional x) (if x (message "-=≕---> %s" x)))


;;; Handler

(defun dupan-handler (operation &rest args)
  (let ((handler (intern (format "dupan-handle:%s" operation))))
    (if (fboundp handler)
        (apply handler args)
      (dupan-run-real-handler operation args))))

(defun dupan-run-real-handler (operation args)
  (let* ((inhibit-file-name-handlers `(dupan-handler
                                       tramp-file-name-handler
                                       tramp-vc-file-name-handler
                                       tramp-completion-file-name-handler
                                       . ,inhibit-file-name-handlers))
         (inhibit-file-name-operation operation))
    (apply operation args)))

(defun dupan-handle:file-exists-p (filename &optional nocache)
  (dupan-info "[handler] file-exists-p: %s" filename)
  (setq filename (dupan-normalize filename))
  ;; 有些插件对这种 tramp 方式的文件访问支持不够好，为避免问题，暂时硬核打补丁
  (cond ((string= "/" filename) t)
        ((string-match-p "~/" filename) nil)
        ((string-match-p "[/.]tags$" filename) nil) ; citre
        (t (dupan--find-with-cache filename nocache))))

(defun dupan-handle:file-readable-p (filename)
  ;; 有些插件对这种 tramp 方式的文件访问支持不够好，为避免问题，暂时硬核打补丁
  (cond ((string-match-p "\\.editorconfig" filename) nil) ; editorconfig
        (t t)))

(defun dupan-handle:file-directory-p (filename)
  (dupan-info "[handler] file-directory-p: %s" filename)
  (when (file-exists-p filename)
    (setq filename (dupan-normalize filename))
    (let ((finfo (dupan--find-with-cache filename)))
      (equal (alist-get 'isdir finfo) 1))))

(defun dupan-handle:file-executable-p (filename)
  (file-directory-p filename))

(defun dupan-handle:file-regular-p (filename)
  (dupan-info "[handler] file-regular-p: %s" filename)
  (not (file-directory-p filename)))

(defun dupan-handle:file-remote-p (file &optional identification _connected)
  (cl-case identification
    ((method) dupan-prefix)
    ((user) "")
    ((host) "")
    ((localname) (dupan-normalize file))
    (t (concat dupan-prefix "/"))))

(defun dupan-handle:expand-file-name (name &optional dir)
  (if (string-prefix-p dupan-prefix name) name
    (dupan-run-real-handler #'expand-file-name (list name dir))))

(defun dupan-handle:file-name-directory (filename)
  (if (string-match (concat "^\\(" dupan-prefix ".*/\\).*$") filename)
      (match-string 1 filename)
    dupan-prefix))

(defun dupan-handle:file-name-nondirectory (filename)
  (if (string-match (concat "^" dupan-prefix ".*/\\(.*\\)$") filename)
      (match-string 1 filename)
    (substring filename 4)))

;; CRUD

(defun dupan-handle:make-directory (dir &optional _parents)
  (setq dir (dupan-normalize dir))
  (dupan-req 'mkdir dir))

(defun dupan-handle:delete-file (filename &optional _trash)
  (setq filename (dupan-normalize filename))
  (dupan-req 'delete filename)
  (sleep-for dupan-sleep-ticks))

(defun dupan-handle:delete-directory (directory &optional _recursive _trash)
  (setq directory (dupan-normalize directory))
  (dupan-handle:delete-file directory))

(defun dupan-handle:copy-file (file newname &optional ok-if-already-exists _keep-time _preserve-uid-gid _preserve-selinux-context)
  (cond
   ((and (dupan-file-p file) (dupan-file-p newname))
    (dupan-req 'copy
               (dupan-normalize file)
               (dupan-normalize newname))
    (message "复制成功!"))
   ((and (dupan-file-p file) (not (dupan-file-p newname)))
    (rename-file (file-local-copy file) newname))
   ((and (not (dupan-file-p file)) (dupan-file-p newname))
    (dupan-req 'upload file (dupan-normalize newname) ok-if-already-exists))))

(defun dupan-handle:copy-directory (directory newname &optional keep-time parents copy-contents incp)
  "参数 INCP 表示开启了增量上传，即代表目标文件夹已经存在时的策略。"
  (cond
   ((and (dupan-file-p directory) (dupan-file-p newname))
    (if parents (make-directory
                 (file-name-directory (directory-file-name newname))
                 parents))
    (copy-file directory newname nil keep-time parents copy-contents))
   ((and (not (dupan-file-p directory)) (dupan-file-p newname))
    (when (directory-name-p newname)
      (setq newname (concat newname (file-name-nondirectory directory))))
    (let ((existp (dupan-handle:file-exists-p newname t)))
      (when (or (not existp) incp (setq incp (y-or-n-p "文件夹已存在，是否增量上传（只上传网盘中不存在的文件）？")))
        (unless existp (make-directory newname t))
        (let* ((locals (directory-files directory 'full directory-files-no-dot-files-regexp))
               (remotes (if incp (dupan-req 'list (dupan-normalize newname)))))
          (dolist (file locals)
            (let ((target (concat (file-name-as-directory newname) (file-name-nondirectory file))))
              (if (file-directory-p file)
                  (dupan-handle:copy-directory file target keep-time parents t incp)
                (when (or (not incp)
                          (not (cl-find-if (lambda (r) (string= (file-name-nondirectory file) (alist-get 'server_filename r))) remotes)))
                  (message "上传文件 %s..." file)
                  (copy-file file target t keep-time)))
              (sleep-for dupan-sleep-ticks)))))))
   ((and (dupan-file-p directory) (not (dupan-file-p newname)))
    (when (directory-name-p newname)
      (setq newname (concat newname (file-name-nondirectory directory))))
    (let ((existp (file-exists-p newname)))
      (when (or (not existp) incp (setq incp (y-or-n-p "文件夹已存在，是否增量下载（只下载本地不存在的文件）？")))
        (unless existp (make-directory newname t))
        (let* ((remotes (dupan-req 'list (dupan-normalize directory)))
               (locals (directory-files newname 'full directory-files-no-dot-files-regexp)))
          (dolist (file remotes)
            (let ((path (alist-get 'path file))
                  (target (concat (file-name-as-directory newname) (alist-get 'server_filename file))))
              (if (equal (alist-get 'isdir file) 1)
                  (dupan-handle:copy-directory (dupan-normalize path t) target keep-time parents t incp)
                (when (or (not incp)
                          (not (cl-find-if (lambda (l) (string= (alist-get 'server_filename file) (file-name-nondirectory l))) locals)))
                  (message "下载文件 %s..." path)
                  (copy-file (dupan-normalize path t) target t keep-time)))
              (sleep-for dupan-sleep-ticks)))))))))

(defun dupan-handle:rename-file (file newname &optional ok-if-already-exists)
  (cond
   ((and (dupan-file-p file) (dupan-file-p newname))
    (dupan-req 'move (dupan-normalize file) (dupan-normalize newname)))
   ((and (dupan-file-p file) (not (dupan-file-p newname)))
    (copy-file file newname ok-if-already-exists)
    (delete-file file t))
   ((and (not (dupan-file-p file)) (dupan-file-p newname))
    (copy-file file newname ok-if-already-exists)
    (delete-file file t))))

;; Contents

(defun dupan-handle:file-modes (&rest _) 492)

(defun dupan-handle:file-attributes (filename &optional _id-format _ometadata)
  (dupan-info "[handler] file-attributes: %s" filename)
  (setq filename (dupan-normalize filename))
  (let* ((finfo (dupan--find-with-cache filename))
         (date (if-let ((time (alist-get 'server_mtime finfo))) (time-convert time) (date-to-time "Mon, 01 Jan 0000 00:00:00 +0000")))
         (folder (equal (alist-get 'isdir finfo) 1))
         (size (or (alist-get 'size finfo) 0))
         (perm (concat (if folder "d" "-") "rwxr-xr--")))
    ;; folder? / links / UID / GID / atime / mtime / ctime / size / perm
    (list folder 1 0 0 date date date (or size 0) perm t nil nil)))

(defun dupan-handle:insert-directory (filename _switches &optional _wildcard full-directory-p)
  "Async, so make `directory-files' return nil, and loading here."
  (dupan-info "[handler] insert-directory: %s" filename)
  (setq filename (expand-file-name filename))

  (if (not full-directory-p)
      (let* ((attrs (file-attributes filename))
             (a (format "  %s %2d %2s %2s %8s %s "
                        (elt attrs 8) (elt attrs 1) (elt attrs 2) (elt attrs 3)
                        (file-size-human-readable (elt attrs 7))
                        (format-time-string "%Y-%m-%d %H:%M" (elt attrs 4))))
             (s (with-temp-buffer
                  (insert (file-name-nondirectory (directory-file-name filename)) "\n")
                  (put-text-property (point-min) (- (point-max) 1) 'dired-filename t)
                  (buffer-string))))
        (goto-char (point-max))
        (with-silent-modifications
          (insert a) (save-excursion (insert s))))

    (message "加载中...")
    ;; usage
    (let* ((quota (dupan-req 'quota))
           (used (alist-get 'used quota))
           (total (alist-get 'total quota)))
      (with-silent-modifications
        (goto-char (point-max))
        (insert (format "  baidu: %s/%s (%.2f%% used)\n"
                        (file-size-human-readable used)
                        (file-size-human-readable total)
                        (/ (* used 100.0) total)))))
    (dupan-req 'list (dupan-normalize filename)
      (lambda (res)
        ;; files
        (setq-local dupan--local-files
                    (cl-loop for f in res
                             for isdir = (equal (alist-get 'isdir f) 1)
                             if isdir collect f into ds
                             else collect f into fs
                             finally return (append ds fs)))
        (cl-loop for f in dupan--local-files
                 do (save-excursion
                      (insert-directory (dupan-normalize (alist-get 'path f) t) nil)))
        ;; progress
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "used)" nil t)
          (with-silent-modifications
            (delete-region (point) (line-end-position))
            (insert (format ", loaded success."))
            (message (if dupan--local-files "加载完成。" "文件夹下无内容。"))))))))

(defun dupan-handle:insert-file-contents (filename &optional visit _beg _end replace)
  (dupan-info "[handler] insert-file-contents: %s" filename)
  (condition-case err
      (let ((count 0))
        (when (file-exists-p filename)
          (let ((nf (file-local-copy filename)))
            (unwind-protect
                (save-excursion
                  (if replace (erase-buffer))
                  (setq count (cdr (insert-file-contents nf nil nil nil t))))
              (delete-file nf))))
        (when visit
          (setf buffer-file-name filename)
          (setf buffer-read-only (not (file-writable-p filename)))
          (set-visited-file-modtime (current-time)))
        (cons filename count))
    (error (kill-buffer) (signal 'user-error (cdr err)))))

(defun dupan-handle:write-region (beg end filename &optional append visit _lockname _mustbenew)
  (dupan-info "[handler] write-region: %s, %s, %s" filename beg end)
  (cl-assert (not append))
  (setq filename (dupan-normalize filename))
  (let ((tmpfile (make-temp-file (file-name-nondirectory filename))))
    (unwind-protect
        (let (create-lockfiles)
          (write-region beg end tmpfile nil 'no-message)
          (dupan-req 'upload tmpfile filename t))
      (delete-file tmpfile))
    (when (stringp visit)
      (set-visited-file-name visit))
    (when (or (eq t visit) (stringp visit))
      (set-buffer-modified-p nil))
    (when (or (eq t visit) (eq nil visit) (stringp visit))
      (message "Wrote %s" filename))
    (setq save-buffer-coding-system buffer-file-coding-system)))

(defun dupan-handle:file-local-copy (filename)
  (dupan-info "[handler] file-local-copy: %s" filename)
  (let ((name (dupan-normalize filename)))
    (dupan-set-ttl-cache (concat "finfo:" name) nil)
    (let ((finfo (dupan-req 'finfo name t)))
      (if (file-exists-p filename)
          (let ((dlink (alist-get 'dlink finfo))
                (newname (concat temporary-file-directory
                                 (make-temp-name (file-name-nondirectory name))
                                 "." (file-name-extension name))))
            (dupan-req 'download dlink newname)
            newname)
        (user-error "要复制的文件 %s 不存在" filename)))))

(defun dupan-handle:dired-insert-directory (dir switches &optional file-list wildcard _hdr)
  (dupan-info "[handler] dired-insert-directory: %s" dir)
  (if file-list
      (cl-loop for file in file-list
               do (insert-directory (concat dir file) switches))
    (insert-directory dir switches wildcard t)))

(defun dupan-handle:load (file &optional noerror nomessage nosuffix must-suffix)
  "Load a remote FILE."
  (let (localfile)
    (condition-case nil
        (setq localfile (file-local-copy file))
      (error (user-error "文件 '%s' 没找到" file)))
    (when (and localfile (file-exists-p localfile))
      (let ((signal-hook-function (unless noerror signal-hook-function))
            (inhibit-message (or inhibit-message nomessage)))
        (unwind-protect
            (load localfile noerror t nosuffix must-suffix))
        (delete-file localfile)))))

(defun dupan-handle:process-file (program &optional _infile _buffer _display &rest args)
  (when-let ((name (and (equal program "file") (equal (car args) "--") (cadr args))))
    (let ((file (dupan-normalize (expand-file-name name)))
          (repeat (equal last-command 'dired-show-file-type)))
      (unless repeat
        (dupan-set-ttl-cache (concat "finfo:" file) nil))
      (let* ((finfo (dupan-req 'finfo file t))
             (dlink (alist-get 'dlink finfo))
             (path (alist-get 'path finfo)))
        (if (equal (alist-get 'isdir finfo) 0)
            (if (not repeat)
                (user-error "文件 `%s' 的 dlink 为:\n\n%s\n\n继续按 y 复制下载命令。" path dlink)
              (let ((durl (funcall dupan-make-download-cmdline-function (dupan-make-url dlink) finfo)))
                (with-temp-buffer
                  (insert durl)
                  (clipboard-kill-ring-save (point-min) (point-max))
                  (user-error "文件 '%s' 的下载命令已复制，请到终端中粘贴使用。" path))))
          (user-error "当前文件夹: %s" file))))))

;; Rude handling

(defun dupan-handle:file-writable-p (_) t)
(defun dupan-handle:file-owner-preserved-p (_) t)
(defun dupan-handle:directory-files (&rest _) nil)
(defun dupan-handle:make-symbolic-link (&rest _) nil)
(defun dupan-handle:add-name-to-file (&rest _) nil)
(defun dupan-handle:dired-compress-file (&rest _) nil)
(defun dupan-handle:find-backup-file-name (&rest _) nil)
(defun dupan-handle:unhandled-file-name-directory (&rest _) nil)
(defun dupan-handle:start-file-process (&rest _) nil)
(defun dupan-handle:shell-command (&rest _) nil)
(defun dupan-handle:executable-find (_) nil)
(defun dupan-handle:vc-registered (&rest _) nil)
(defun dupan-handle:set-file-modes (&rest _) nil)
(defun dupan-handle:set-file-times (&rest _) nil)
(defun dupan-handle:set-visited-file-modtime (&rest _) nil)
(defun dupan-handle:verify-visited-file-modtime (&optional _buf) t)
(defun dupan-handle:file-selinux-context (&rest _) nil)


;;; Patches

(defun dupan-dired-create-files-advice (fn file-creator operation fn-list name-constructor &optional marker-char)
  "优化在 Dired 中 Mark 多个文件后进行的复制或移动操作。
默认情况是在一个循环中反复调用 dired-do-xxx 方法，这样容易导致抛出 '调用频繁' 的错误，所以利用已有 API 改成批量处理。"
  (if (and (> (length fn-list) 1)
           (dupan-file-p (car fn-list))
           (dupan-file-p (funcall name-constructor (car fn-list))))
      (let* ((len (length fn-list))
             (first (funcall name-constructor (car fn-list)))
             (dir (dupan-normalize (file-name-directory first)))
             (files (mapcar #'dupan-normalize fn-list))
             (check-fn (lambda (files dir)
                         (cl-loop with fs = (dupan-req 'list dir)
                                  for f in files
                                  for n = (file-name-nondirectory f)
                                  for p = (expand-file-name n dir)
                                  if (cl-find-if (lambda (x) (string= (alist-get 'path x) p)) fs)
                                  collect n into dups
                                  finally (when dups
                                            (user-error
                                             "目标文件夹中已存在文件: %s。操作中止。"
                                             (mapconcat #'identity dups "、"))))
                         (message "总共 %d 个文件，请等待..." (length files))))
             (refresh-fn (lambda (&optional movep)
                           (dolist (f fn-list)
                             (condition-case nil
                                 (dired-add-file (expand-file-name (file-name-nondirectory f) (file-name-directory first)))
                               (error (user-error "操作成功。显示出错，按 'g' 手动刷新 Dired 文件夹。")))
                             (if movep (dired-remove-file f))
                             (sleep-for 0.3))))) ; wait for ?
        (dupan-info "[dired-create-files-advice] %s, %s, %s, %s" file-creator operation files first)
        (cond ((and (equal file-creator 'dired-copy-file) (string= operation "Copy")) ; 多个文件的复制
               (funcall check-fn files dir)
               (dupan-req 'copy files dir)
               (funcall refresh-fn))
              ((and (equal file-creator 'dired-rename-file) (string= operation "Move")) ; 多个文件的移动
               (funcall check-fn files dir)
               (dupan-req 'move files dir)
               (funcall refresh-fn t))
              (t (user-error "未知操作: dired-create-files - %s, %s" file-creator operation)))
        (dired-move-to-filename)
        (message "完成 (%s/%s)" len len))
    ;; 默认逻辑
    (funcall fn file-creator operation fn-list name-constructor marker-char)))

(advice-add #'dired-create-files :around #'dupan-dired-create-files-advice)

(defun dupan-dired-sort-toggle-advice (fn)
  "定义 dupan 文件的排序逻辑，使用 `completing-read' 的方式进行交互选择。"
  (if (and default-directory (dupan-file-p default-directory))
      (let* ((norm (lambda (s) (concat (car s) (if (cdr s) " desc"))))
             (oldsort (or dupan--sort-type (car dupan--sorts)))
             (choose (completing-read "切换到排序方式: "
                                      (cl-loop for s in dupan--sorts
                                               unless (equal s oldsort)
                                               collect (funcall norm s))
                                      nil t))
             (newsort (cl-find-if (lambda (s) (string= (funcall norm s) choose)) dupan--sorts)))
        (setq-local dupan--sort-type newsort)
        (setq mode-name (concat "Dired by "
                                (car dupan--sort-type)
                                (if (cdr dupan--sort-type) " desc")))
        (force-mode-line-update)
        (revert-buffer))
    (funcall fn)))

(advice-add #'dired-sort-toggle :around #'dupan-dired-sort-toggle-advice)

(defun dupan-project--find-in-directory-advice (fn dir)
  "避免通过搜索 vcs 文件的方式获取 current-project，提高效率"
  (if (dupan-file-p dir)
      (cons 'transient dupan-prefix)
    (funcall fn dir)))

(advice-add #'project--find-in-directory :around #'dupan-project--find-in-directory-advice)



;;;###autoload
(defun dupan-find (&optional choose-dir)
  "搜索网盘文件并打开。如果带命令前缀 (即 C-u) 那么将提示搜索的目录。"
  (interactive "P")
  (let ((input (read-string "搜索关键字: ")))
    (when (< (length (replace-regexp-in-string " " "" input)) 1)
      (user-error "是不是输入的内容太少了？"))
    (let* ((dir (when choose-dir
                  (let* ((def (if (dupan-file-p default-directory) (dupan-normalize default-directory)))
                         (sel (read-string "搜索的文件夹: " def)))
                    (cond ((or (null sel) (string= sel "/") (string= sel "")) nil)
                          (t (if (string-prefix-p "/" sel) sel (concat "/" sel)))))))
           (desc (if dir (concat " in '" dir "'") ""))
           (prompt (format "网盘文件%s (关键词 %s): " desc input)))
      (message "搜索%s..." desc)
      (let* ((files (dupan-req 'search input (dupan-normalize dir)))
             (candicates (cl-loop
                          for file in files
                          when (equal (alist-get 'isdir file) 0)
                          collect (dupan-normalize (alist-get 'path file) t))))
        (when (< (length candicates) 1)
          (user-error "在网盘%s 没找到 '%s' 有关内容" desc input))
        (let ((f (completing-read prompt candicates)))
          (if (dupan-file-p f)
              (find-file f)
            (user-error "你要打开的 '%s' 并不是一个网盘文件吧？" f)))))))

;;;###autoload
(defun dupan-browser (&optional dir)
  "在浏览器中打开网盘页面。"
  (interactive)
  (let* ((url "https://pan.baidu.com/disk/main#/index?category=all&path=")
         (dir (or dir (if (dupan-file-p default-directory) (dupan-normalize default-directory))))
         (path (read-string "在浏览器中打开目录: " dir)))
    (unless (string-prefix-p "/" path) (setq path (concat "/" path)))
    (message "已在浏览器中打开 '%s'，请前往查看。" path)
    (browse-url (concat url (url-hexify-string path)))))

(defvar dupan-aria2-rpc-url "http://localhost:6800/jsonrpc")
(defvar dupan-aria2-rpc-token nil)

(defun dupan-download-with-aria2 ()
  "将当前网盘文件通过 rpc 发送给 aria2 进行下载"
  (interactive)
  (let ((file (dired-file-name-at-point)))
    (if (and file (file-regular-p file))
        (let* ((finfo (dupan-req 'finfo (dupan-normalize (expand-file-name file)) t))
               (out (let ((p (string-trim (read-string "下载当前文件并命名为: "
                                                       (file-name-nondirectory (alist-get 'path finfo))))))
                      (if (> (length p) 1) p (error "需要提供一个有效名字"))))
               (url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "application/json")))
               (url-request-data (encode-coding-string
                                  (json-serialize
                                   `((jsonrpc . 2.0)
                                     (id . ,(format "dupan:%d" (time-to-seconds)))
                                     (method . "aria2.addUri")
                                     (params . ,(vconcat
                                                 (delq nil
                                                       (list
                                                        (if dupan-aria2-rpc-token (format "token:%s" dupan-aria2-rpc-token))
                                                        (vconcat (list (dupan-make-url (alist-get 'dlink finfo))))
                                                        (list :out out)))))))
                                  'utf-8))
               url-history-track json-response)
          (dupan-info "rpc-cmdline data: %s" url-request-data)
          (with-current-buffer (url-retrieve-synchronously dupan-aria2-rpc-url t)
            (goto-char (point-max))
            (beginning-of-line)
            (setq json-response (json-parse-buffer :object-type 'alist))
            (kill-buffer))
          (if (alist-get 'result json-response)
              (message "已经将下载任务添加到了 aria2 中")
            (error "添加到 aria2 失败: %s" (alist-get 'message (alist-get 'error json-response)))))
      (user-error "请确保光标下面是一个百度云盘文件"))))

(add-to-list 'file-name-handler-alist
             `(,(concat "\\`" dupan-prefix) . dupan-handler))

(provide 'dupan)

;;; dupan.el ends here
