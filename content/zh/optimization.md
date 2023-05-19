---
title: 插件：功能优化类
weight: 5
---

接下来的部分，我们会开始介绍一些实用插件，让 Emacs 变得强大起来。在开始之前读者需要阅读完上一篇教程，知道如何进行 Emacs 的配置。

由于插件数量众多，本教程将分类进行介绍。大体分为如下几类：

1. 功能优化类：对 Emacs 自身的一些不够完美的功能进行替换，解决一些痛点，提高操作便利性
2. 功能增强类：大大提升 Emacs 体验与效率
3. 编程类：和编程相关的插件配置
4. 外观类：配置颜色、主题、屏保等。

值得一提的是，在众多插件中，其实相同的功能可能有好几个插件来实现，最典型的就是有两个知名度很高的插件做的是几乎同一件事。类比一下正如 Emacs 和 Vim、肯德基和麦当劳。本教程将重点介绍其中的一个笔者常使用的插件，对另一个插件（如果笔者了解的话）会做一点提及，读者可以自行进一步了解并根据自己的偏好进行选择。

> 在这部分插件篇中，如果读者有除了文章中提到的插件以外的相关插件想要推荐给大家使用、对文章作出补充，欢迎在 [Github](https://github.com/Pavinberg/emacs-book) 中提出 Pull Request。

第一个插件笔者会详细介绍一下每一步的细节，后面的插件会逐步省略。

{{< tip >}}
本教程对插件的功能介绍大部分局限于核心功能，其它功能会省略，主要目的还是希望读者能抓住重点，不必劳烦记住所有繁琐的功能。在读者使用的过程中，可以自行探索插件的其它功能，一是可以查阅插件的文档，二是可以直接在 Emacs 内查看函数的文档。插件相关的命名都是会使用插件名作为前缀，例如马上要介绍的插件 `ivy`，和它相关的命令就都是 `ivy-*` 这样的命名。所以读者在安装 `ivy` 后可以首先 `M-x` `ivy-` 然后就可以看到一系列以此为前缀的命令，想要查询其中某一个的功能例如 `ivy-push-view`，那就先 `C-g` 回到正常 Buffer 内，然后输入我们第二篇教程就介绍的命令 `C-h f` `ivy-push-view` 就可以显示这个命令的介绍了。
{{< /tip >}}

## ivy

### 简介

[主页](https://github.com/abo-abo/swiper)

首先要介绍的当属 `ivy` 了。读者在前面使用 Emacs 时，可能多少会感到就像一开始使用 Linux 中的初始 shell
Bash，功能是全的，但是使用的友好度差了点。`ivy` 就为 Emacs 带来了使用体验上的巨大提升，就好像用了 `oh-my-zsh` 的 zsh。

打开 `ivy` 的链接，会发现其实这个仓库名为 swiper，里面包含了 `ivy`、`counsel` 和 `swiper` 三部分。它们三个分别加强了 Emacs 的三个方面： **补全系统** 、 **部分常用命令** 、 **搜索功能** 。由于是作者 abo-abo（Oleh
Krehel）开发的三个模块，现在都整合到了一起，我们一并安装就好。顺带一提的是，这位作者非常厉害，我们后面提到的好几个插件都由他发起和编写。

### 配置流程

接下来，通过 `ivy` 来让读者熟悉一下 Emacs 安装插件的流程：

  1.  **得知插件** ：首先我们会通过一些渠道听说某个插件很好用（比如通过本系列教程），于是我们找到它的主页（绝大多数情况都是一个 GitHub 仓库，我们可以 Google 一下或者直接在 GitHub 上搜索）。
  2.  **阅读主页** ：一个好用且用户数量大的插件的主页一定是非常完善的，包括了插件的功能介绍、安装、配置，以及 Issue （Issue 指使用中上报发现的问题和漏洞）和如何贡献等等。对于我们 Emacs 初级用户而言，我们主要关注前半部分，也就是功能、安装、配置。
  3.  **安装** ：尽管该仓库包含了三部分，但它们都合并到了一个 package，也就是相当于是一个插件，名为 `ivy`。于是我们看 `ivy`的主页的 “Installation” 一节中写着：

> Install the ivy package from GNU ELPA or MELPA.

很多插件都会用这样的一句话一笔带过安装方式。那么这就是对应了[上一篇教程中提到的 MELPA](../configurations#melpa) 的相关内容了。根据上一篇的介绍，MELPA 是一个插件仓库，那么只要我们的配置文件中写好了相应的链接配置，就可以通过输入 `M-x` `package-install` `<RET>` `ivy` `<RET>` 来安装插件了。

但此外，在上一篇教程中还提到了，我们有一个更方便的方式来管理插件—— `use-package` 插件。因此读者需要首先按照上一篇教程中的指导安装好 `use-package` 插件。于是我们可以直接在配置文件（例如 `~/.emacs.d/init.el`）中写上：
    
```elisp
(use-package ivy
  :ensure t)
```	  

表示了希望确保 `ivy` 这个 package 已被安装，否则就下载安装 ivy。然后可以通过重启 Emacs 生效，或者直接运行这句刚加入的配置代码：首先选中要运行的这部分代码，然后输入 `M-x` `eval-region` ，或者光标在括号内时按下 `C-M-x` （eval-defun 执行函数）。之后 `use-package` 就会自动为你安装好 `ivy`。

4. **配置** ：主页上提供了完整的配置文档，同时也提供了一个简单的配置，那么一开始我们就直接使用它提供好的典型配置就好。但注意，它的写法是没有使用 `use-package` 的，有的插件会提供 `use-package` 的写法，而有的不会，这一点纯粹是依靠文档编写者的喜好。因此我们要填入我们自己的 `use-package` 中。事实上这个过程非常简单，我们首先看一下它提供的配置（笔者加入了一些注解）：

```elisp
;; ---- 执行了一个函数启动 ivy mode ----
(ivy-mode)
;; ---- 设置两个变量为 True，还有一个可选的 ---
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
;; ---- 绑定快捷键 ----
(global-set-key "C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
```

那么想要转移到 `use-package` ，只需要把这些代码根据逻辑抄一遍就好了。还记得我们说 `use-package` 中 `:init` 标签表示加载插件前执行的逻辑（如，启动 `ivy-mode`），`:config` 表示加载后执行的逻辑（如，设置一些变量，这显然是加载插件后的逻辑，因此写在 `:config` 后面）。至于绑定快捷键，只需要照抄一下。最后一行的配置涉及到了 keymap，keymap 就是指在某一模式下的键盘输入事件的集合。例如我们平时输 `C-x` 后 Emacs 并不会进行任何操作，而是等待你输入下一个快捷键，原因就在于 `C-x` 是当前 keymap 的一个快捷键前缀，尚未形成一个命令，所以要继续等待。最后一行的配置表示在 `minibuffer-local-map` 这个 keymap 内重新定义 `C-r`。在 `use-package` 这个也是可以同样配置的。

特别的，注意到官网 [Counsel](https://github.com/abo-abo/swiper#counsel) 中提示了可以直接通过执行 `counsel-mode` 来绑定快捷键，所以我们当然也可以利用起来，最后的代码只需要这样：

```elisp
(use-package counsel
  :ensure t)
    
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))
```

将以上代码写在 `init.el` 中，就可以加载 `ivy` 包（如未安装就下载安装），在加载前（`:init` 标签）启动 `ivy-mode`、`counsel-mode`，加载后（`:config` 标签）设置三个变量的值、绑定一些额外的快捷键（`:bind` 标签）并针对 keymap 设定（ `:bind` 标签内的 `:map` 标签）。

{{< tip >}}
直接使用 use-package 来安装 ivy、counsel 和 swiper 可能存在一些依赖问题，笔者暂未解决。对此可以手动安装，例如 `M-x` `package-install` `counsel`。
{{< /tip >}}

### 效果

![ivy](../../images/emacs-book/optimization/ivy-find-file.png)

例如，按下 `C-x C-f` 时，我们不再调用的是 `find-file` ，而是 `counsel-find-file`，最主要的区别便是会把当前目录下所有文件列表显示在一个新的 Minibuffer 中，当你想到上一级目录时，也只需要按下一次 `<backspace>` ，而不用像之前一样要删除一个词。 此外，还可以模糊输入文件名就可以找到相应的文件，若按下 `C-r` 则可以显示之前调用 `counsel-find-file` 打开过的历史记录。

同样的，`M-x` 也被替换为了 `counsel-M-x`，会将命令也列表显示出来，同样也可以模糊输入命令。

`C-x b`切换 Buffer 、`C-s` 搜索、`M-y` 使用剪贴板历史等等，都会提供类似的补全功能，读者可以自行多尝试尝试！

{{< tip >}}
在执行 `counsel-find-file` 过程中，当输入了一个文件名前缀时，`ivy` 会自动为你补全最接近的文件名，这在想要创建文件时反而引入了麻烦。例如上图中我们已经有了 `names.txt` 文件，此时如果想要创建一个名为 `names` 的文件的话，输入 `names` 后按下回车，`ivy` 会认为你在找 `names.txt` 而直接打开它。 对于这种情况，需要按下 `C-M-j` （`ivy-immediate-done` 命令），就会直接创建名为 `names` 的文件而不触发补全。
{{< /tip >}}

### 同类插件

和 `ivy` 功能基本类似的插件有 [helm](https://emacs-helm.github.io/helm/) 和 [ido](https://github.com/DarwinAwardWinner/ido-completing-read-plus) ， 读者可以自己选择自己更为喜爱的。

## amx

[主页](https://github.com/DarwinAwardWinner/amx)

这个插件可以记录我们每次调用 `M-x` 时输入的命令历史，然后每次将最常用的显示在前面，这对于我们短时间希望频繁输入某几个命令的场景非常有用。`amx` 的配置十分简单：
   
```elisp
(use-package amx
  :ensure t
  :init (amx-mode))
```

##  ace-window

[主页](https://github.com/abo-abo/ace-window)

这又是一个 abo-abo（Oleh Krehel）的项目。我们用 Emacs 多窗口时，window 超过 3 个后就很难使用 `C-x o` 进行切换了。`ace-window` 对 `C-x o` 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转。

![ace-window](../../images/emacs-book/optimization/ace-window.png)

配置：

```elisp
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))
```

## mwim

[主页](https://github.com/alezost/mwim.el)

还记得我们提到 `C-a` 对应了 `move-beginning-of-line`，`M-m` 对应了 `back-to-indentation`。当代码有缩进时，前者会把光标移动到行首（到空格之前），后者会移动到代码文字的开头（到空格之后）。那么实际中这两个按法差别较大，且不易区分，使用起来不方便。`mwim` 就将二者合并，覆盖 `C-a` 为 `mwim-beginning-of-code-or-line`，这样按一次 `C-a` 时移动到代码文字开头，再按一次则是移动到整行的行首，如此反复。

同时，更有意义的是，它还可以覆盖 `C-e` `move-end-of-line` 为 `mwim-end-of-code-or-line`，当本行代码结尾有注释时，第一次按 `C-e` 将光标移动到代码尾部、注释之前。再按一次则是移动到整行的行尾。 这就大大提高了写代码的效率。

```elisp
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))
```

## undo-tree

[主页](https://www.emacswiki.org/emacs/UndoTree)

还记得 Emacs 令人头疼的撤销和重做操作吗？`C-x u` 对应了 `undo` 命令，而 `redo` 则需要先 `C-g` 让历史记录环方向转换，再进行 `undo` 表示 `redo`。 事实上这一设定的原理也很直白，有一个表情包吐槽了这一设定：

![undo-tree-meme](../../images/emacs-book/optimization/undo-the-undo.jpeg)

翻译一下就是：想要撤销之前的撤销？你如果撤销，然后再做一个没有任何内容编辑的行动，之后下一个撤销就会撤销刚刚的撤销，于是形成了一个重做。`C-g` 就是起到其中“没有任何内容编辑的行动”的作用。

事实上这也的确让很多人头疼。于是自有人写了插件来弥补。只需一张图，你即可了解 undo-tree 的用法：

![undo-tree](../../images/emacs-book/optimization/undo-tree.png)

所有的编辑都会成为 undo-tree 上的一个节点。当按下 `C-x u` 时就会显示出这棵树。当你撤销时，只需要向上寻找历史节点。而当你又做了其它编辑，这棵树就会分叉。也就意味着，你之前的记录依然被保留，你随时可以反悔，也不用担心不小心编辑了内容导致无法重做。

undo-tree 被放在了 GNU ELPA 上，并不是 MELPA，所以读者如果用了国内镜像，一定要把 GNU ELPA 加入到包管理链接中，详见[上一篇教程的 MELPA 章节](../configurations#melpa)。

配置方面，简单的使用只需要如下配置。然而其默认会为每个文件生成一个隐藏文件用来保存之前的历史记录，这对项目是个污染。因此，最后的 `:custom` 中设置了变量 `undo-tree-auto-save-history` 为空，就是关闭了这个保存功能。此外，也可以将所有的 `undo-tree` 历史记录保存到一个专门的文件夹，需要通过变量 `undo-tree-history-directory-alist` 来设置，读者详见文档。 

```elisp
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))
```

## smart-mode-line（可选）

[主页](https://github.com/Malabarba/smart-mode-line)

一个让 mode line 更加漂亮、方便管理的插件，可以自动做一些模式的隐藏等等，也可以选择多种主题。具体读者可以自行探索。这里给个基础配置，在教程的[外观与主题](../theme#mode-line)中会额外介绍一部分配置：
    
```elisp
(use-package smart-mode-line
  :ensure t
  :init (sml/setup))
```

## good-scroll （可选）

[主页](https://github.com/io12/good-scroll.el)

在现代图形界面操作系统中，光标在上下移动、翻页的时候 Emacs 会直接刷新界面，滚动时也是按行滚动，比较粗糙。`good-scroll` 提供了平滑滚动，并且支持变速滚动，更加顺手。

```elisp
(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))
```

---

经过了以上介绍，相信读者应该能够感受到 Emacs 的不少痛点被解决了，现在开始 Emacs
应当是一个可以顺畅使用的现代编辑器了。下一篇笔者将介绍一些功能增强类插件，开始真正释放 Emacs 的强大。
