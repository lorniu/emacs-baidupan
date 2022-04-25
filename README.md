[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## 百度网盘客户端 on Emacs

搞 https://github.com/lorniu/emacs-dropbox 的时候，顺手写的这个。

可以使用它进行网盘文件的进行基本管理:
- 通过 `Dired` 方式进行基本的查看、移动、拷贝、上传、下载等
- 使用类似 `Tramp` 的方式对文件进行修改

由于百度的 API 实在有些一言难尽，自己平日百度网盘用的也不多，有一些功能没有实现或暂时不完善，包括但不限于：
- 对于超大文件的下载，没做更好的处理（Emacs 单线程会阻塞啊，最好将下载任务分发到外部程序比如 wget/aria2 等中进行。不过好像也没啥意义）
- 视频、音频、图片的管理，没能做的更好（相册的管理啦、通过流的方式直接播放视频啦...并不是我需求）

U1S1，用于阅读文档、简单编辑、文件整理等，这个比官方的客户端要强太多了 :)

仓促写就，优化不够。如果有人使用，感谢提 BUG、提意见或提补丁。

## 使用指南

  1. 下载本文件，并加载之 (通过 `load` 或 `require`)
  2. 确保拥有百度帐号，执行 `M-x dupan-gen-config` 命令生成记录身份信息的配置文件
  3. 之后，使用 `M-x dupan-find` 搜索网盘文件并打开。亦或使用 `C-x C-f /dp:/网盘文件路径` 直接打开指定文件。

## 相关链接

- 百度开放平台文档: https://pan.baidu.com/union/doc
- 百度开放平台控制台: https://pan.baidu.com/union/console

## 其他

草草实现，未尽其精。随君自取，维护随缘。如此而已。
