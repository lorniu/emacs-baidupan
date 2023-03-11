[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## 百度网盘客户端 on Emacs

搞 https://github.com/lorniu/emacs-dropbox 的时候，顺手写的这个。

可以使用它进行网盘文件的进行基本管理:
- 通过 `Dired` 方式进行基本的查看、移动、拷贝、上传、下载等
- 使用类似 `Tramp` 的方式对文件进行修改
- 充分利用 Emacs 的优势，可以跨平台

由于百度的 API 实在有些一言难尽，自己平日百度网盘用的也不多，有一些功能没有实现或暂时不完善，包括但不限于：
- 对于超大文件的下载，没做更好的处理（Emacs 单线程会阻塞啊，最好将下载任务分发到外部程序比如 wget/aria2 等中进行。不过好像也没啥意义）
- 视频、音频、图片的管理，没能做的更好（相册的管理啦、通过流的方式直接播放视频啦...并不是我需求）

U1S1，用于阅读文档、简单编辑、文件整理等，这个比官方的客户端要强太多了 :)

随君自取，维护随缘。如果有人使用，欢迎提 BUG、提意见或提补丁。如此而已。

## 使用指南

  1. 下载本文件，并加载之 (通过 `load` 或 `require`)
  2. 确保拥有百度帐号，执行 `M-x dupan-add-account` 添加帐号信息到配置文件
  3. 之后，使用 `M-x dupan-find` 搜索网盘文件并打开。亦或使用 `C-x C-f /dp:/网盘文件路径` 直接打开指定文件
  4. 你可以继续执行 `M-x dupan-add-account` 添加另外的帐号，并使用 `M-x dupan-switch-account` 进行切换

  在打开的 Dired 中:
  - 可以使用 `C` 完成复制、上传、下载，当前支持文件夹的增量式上传下载
  - 使用 `s` 对文件列表进行排序
  - 使用 `y` 显示 `dlink`，再次按 `y` 会将复制下载命令。可以使用 `dupan-make-download-cmdline-function` 定制下载命令行
  - 如果你已经启动了 aria2c rpc 服务，那么可以通过 `M-x dupan-download-with-aria2` 对当前文件进行下载

## 相关链接

- 百度开放平台文档: https://pan.baidu.com/union/doc
- 百度开放平台控制台: https://pan.baidu.com/union/console
