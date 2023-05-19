---
title: 实用小技巧 
weight: 9
---

本文将介绍一些除编程本身外 Emacs 作为一个代码编辑器还能为开发者做什么样的事情，包括：

  * 笔记类任务：Markdown、LaTeX、`org-mode` 及中文编辑
  * 计算器： `calc`
  * Emacs server
  * ChatGPT 协作

> 本文可能会不断更新。

## 笔记

### Markdown

笔记类任务现在比较流行 Markdown，一种非常简单易学的标记语言。

{{< tip >}}
程序员们经常与 Markdown 打交道，所以如果你是一位新人程序员还不了解 Markdown，强烈建议你学习一下。
{{< /tip >}}

Markdown 如今已十分流行，Markdown 编辑器数不胜数，各具特色。要说 Emacs 有什么能完全胜出其它 Markdown 编辑器的优势，笔者认为可能只有下文提到的“中文支持”功能。此外，对于大部分程序开发而言，Markdown 主要是参与写一些 `README.md` 等文档，这样如果代码都是在 Emacs 中编写和浏览，文档也用 Emacs 来写会更为方便、操作更为统一。

因此，笔者对 Emacs 中的 Markdown 的态度是轻度使用，不希望费力配置，甚至不想记太多操作，所以本文只介绍 Emacs 自带的功能 `markdown-mode`。

无需任何配置，打开 `.md` 文件就会自动使用 `markdown-mode`，Emacs 会提供一些语法高亮，还有一些语法的快捷键。例如插入代码块可以输入命令 `M-x` `markdown-insert-gfm-code-block` 然后输入想要输入的编程语言名字即可，其快捷键是 `C-c C-s C`。所有相关命令都是以 `markdown-insert-` 做前缀的， 读者按需记几个常用的就好，毕竟 Markdown 语法已经很简单了，也没必要记太多快捷键。

![markdown](../../images/emacs-book/tricks/markdown.png)

此外还有一些方便跳转的快捷键，例如：

- `C-c C-n` 调用 `markdown-outline-next` 可以快速跳到下节，`C-c C-u` 调用 `markdown-outline-up` 跳到上一节。
- `C-c C-f` 调用 `markdown-outline-next-same-level` 跳到下一个同级章节，`C-c C-b` 调用 `markdown-outline-previous-same-level` 跳到上一个同级章节。 

其它功能的命令也都以 markdown- 为前缀，读者可以自行探索。

想要预览 Markdown，可以输入 `M-x` `markdown-live-preview-mode` 或输入快捷键 ` C-c C-c l` 就可以在另一个窗口里预览 。这个功能应当至少需要 Emacs >= 26 的版本（未完全确认）。笔者认为自带的预览功能比较一般，所以只适合轻度使用。

如果确实认为自带的功能很简陋，读者可以尝试 [Wiki](https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type) 中提及的 `impatient-mode`、`livedown-mode` 和 `realtime-preview`。`impatient-mode` 根据介绍是一个基于 Emacs Lisp 的为 HTML 提供预览的工具，但可以稍微改造一下用于 Markdown 的预览。 其它两个模式分别需要依赖 `Node.js` 和 `Ruby` 这类外部工具，读者按需选择就好。

### LaTeX

LaTeX 是一个功能极其强大的排版系统，一般来说日常无需使用如此重量级的工具，笔者仅在写一些比较重要的报告和论文时才会使用。如果没有这类需求可直接略过本节。

{{< tip >}}
事实上 LaTeX 的编辑器也很多，包括 VSCode 也可以提供良好的体验。笔者使用 Emacs 作为 LaTeX 编辑器其实更多是为了结合下文提到的“中文支持”功能，用以编写中文的 LaTeX。其余功能与 VSCode 大同小异。
{{< /tip >}}

首先应当正常手动安装好 LaTeX 并保证能正常使用。Emacs 中则需要安装 AUCTeX，一个专门的包。可以通过 `M-x` `package-install` `<RET>` `auctex` 安装。

随后打开 `.tex` 文件时会进入 `TeX-mode`（大小写敏感）， 提供了语法高亮。一些可以辅助插入特定格式的快捷键以 `latex-` 为前缀，读者自行探索就好。

LaTeX 编译成的 PDF 也可以在 Emacs 中打开。需要安装 [pdf-tools](https://github.com/vedang/pdf-tools) 插件。依然建议从 MELPA 安装比较方便，可使用如下配置：

```elisp
(use-package pdf-tools
  :ensure t
  :init
  (pdf-loader-install))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(defun pdf-view-kill-rmn-ring-save ()
  "Copy the region to the `kill-ring' after remove all newline characters."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (replace-regexp-in-string "\n" " "
        (car (pdf-view-active-region-text)))))
    (pdf-view-deactivate-region)
	(kill-new txt)))

(use-package pdf-view-mode
  :bind
  ("C-c C-w" . pdf-view-kill-rmn-ring-save))
```

安装后即可在 Emacs 中打开 PDF 文件，同时笔者定义了一个函数 `pdf-view-kill-rmn-ring-save` 并绑定到快捷键 `C-c C-w` 上，该命令可以在选中 PDF 的一段文字后将其复制出来，同时去掉其中的回车。

`pdf-tools` 提供了很多可以快速浏览 PDF 的快捷键，可参考[官方主页](https://github.com/vedang/pdf-tools#keybindings-for-navigating-pdf-documents)。

编译 LaTeX 可以在 `.tex` 文件上按下 `C-c C-c` 然后选择 "LaTeX"。选择 "BibTeX" 或 “Biber” 可以编译引文 `.bib`。切换编译引擎可以设置 `TeX-engine` 变量，其变量文档中有列出可选值，如 “xetex” 可以切换成 XeLaTeX。由于编译是直接编译当前 TeX 文件，对于有多个 `.tex` 文件的项目，可以设置一个主 TeX 文件，对应 `TeX-master` 变量。这些可以利用[插件：编程开发](../development#局部变量)提到的局部变量来进行设置。例如这样的 `.dir-locals.el`：

```elisp
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((latex-mode . ((TeX-engine . xetex)
		(TeX-master . "/path/to/main/tex"))))
```

### 中文支持

这里主要是指对中文内容提供两点支持：

1. 中文分词：Emacs 可以做到 `M-b`、`M-f` 来按中文词进行光标移动。这种支持对于快速编辑中文、保持 Emacs 的操作一致性非常重要。默认情况下，对一段中文文本按下 `M-b`/`M-f` 会直接跳到上一个/下一个标点符号，失去了以词为单位移动的意义。
2. 中英文符号的自定义切换。如果读者写过带公式、代码块的中文 Markdown/LaTeX，一定会发现有一件事非常非常令人厌烦，就是中英文输入法的切换。在输入内容和标点时需要中文字符，但在输入符号时需要英文字符，经常写起来会被这些切换搞得头脑混乱。虽然可以利用编辑器的快捷键缓解问题，但不具有通用性，不尽如人意。

 **解决分词问题** 笔者使用了 [emacs-chinese-word-segmentation](https://github.com/kanglmf/emacs-
chinese-word-segmentation) 工具。该工具借助 jieba 分词实现了分词功能。这个插件没有放到 MELPA 等仓库，需要手动
clone 安装，参见其主页即可。

笔者建议在 `~/.emacs.d/chinese/` 等目录下 clone 该项目，然后按照其主页的命令使用 make 进行编译。随后把其主页的配置内容放到合适的配置文件中去。在想要使用这个中文分词时，手动调用 `cns-mode` 就可以了。如果想要在某个特定项目中使用，那么可以加入到局部变量 `.dir-locals.el` 中，如：

```elisp
((markdown-mode . ((eval . (cns-mode)))))
```

{{< tip >}}
Emacs 还有一个 [pyim](https://elpa.gnu.org/packages/pyim.html) 插件，是一个内部中文输入法，能支持五笔、双拼等，安装了它后，也可以获得分词功能。但是 `pyim` 体验一般，而且毕竟是独立于系统的输入法的，在 Emacs 内外切换两个输入法不是很舒适，用户完全可以使用外部的选择众多的功能强大的输入法搭配 `cns-mode`，因此笔者并不推荐 `pyim`。
{{< /tip >}}

而 **解决输入法切换问题** ，可直接利用 Emacs 的 [keyboard-translate](https://ftp.gnu.org/old-gnu/Manuals/emacs-20.7/html_node/emacs_457.html) 功能，该功能可以实现把一个符号映射成另一个符号。笔者针对 Markdown 使用了如下设置：

```elisp
(add-hook 'markdown-mode-hook (lambda ()
			        (keyboard-translate ?¥ ?$)
				(keyboard-translate ?· ?`)
				(keyboard-translate ?～ ?~)
                                (keyboard-translate ?、 ?\\)
				(keyboard-translate ?｜ ?、)
				(keyboard-translate ?\「 ?{)
				(keyboard-translate ?\」 ?})
				(keyboard-translate ?\《 ?<)
				(keyboard-translate ?\》 ?>)))
```

思路就是一些几乎不使用的中文字符直接映射成英文字符，以方便编写 Markdown。例如第一句，把人民币 `¥` 映射成美元 `$`，这样在中文输入时也可以直接按 `shift+4` 来输入 `$` 符号，从而输入内联公式。同样的， `·` 也被映射为 ``` `` 由此方便输入行内代码。 为了能够方便输入反斜杠，把顿号 `、` 映射为 `\` 而把 `｜` 映射会 顿号 `、`（读者如果不喜欢这样操作可以去掉相应代码）。以此类推。

这里的语法就是“问号+字符”，部分字符需要用反斜杠转义。

### org-mode

[org-mode](https://orgmode.org/) 其实是 Emacs 中非常强大的笔记模式，在 Emacs 中使用的体验、功能的强大远超 Markdown。但笔者目前使用较少，后续会更多尝试。`org-mode` 的主要缺点是一方面需要一定的学习成本，另一方面只能在 Emacs 中使用，不方便直接分享。但笔者认为有必要向读者简单介绍这个功能，因为喜欢 `org-mode` 的人爱不释手，甚至有人使用 Emacs 就是为了 `org-mode`，读者可以自己判断是否适合自己。

读者可以把 `org-mode` 理解成一个 Emacs 内的高级 Markdown，可以带很多复杂的逻辑，然后 Emacs 为它提供了很多方便的功能，例如：

- 章节折叠、跳转
- 方便的多状态、多层级 Todo List
- 丰富多样的链接引用功能
- 表格操作（甚至可以像 Excel 一样输入公式自动计算）
- 插入代码
- 导出成各种格式（Markdown、HTML、LaTeX 等）
- 利用 Emacs 的扩展性扩展更多丰富的功能

只要打开后缀名为 `.org` 的文件就会自动启动 `org-mode`。 主要可以用来做 Todo List、项目记录等。此外，通过安装一些插件和使用内置的一些功能，可以得到更为完整的笔记体验，例如 Zettelkasten 笔记法等。可以类比一下比较有名的笔记软件例如 Notion、Obsidian 等。

具体如何使用笔者就不信口开河了，建议读者查看官方文档。

## 计算器

写程序也是偶尔需要计算器的，例如算一算字节数、传输速率、做一做十六进制转化等。Emacs 内置了一个非常强大的计算器：[calc](https://www.gnu.org/software/emacs/manual/html_node/calc/index.html)。除了常见普通计算器的功能，还包括：

- 复数运算
- 2～36 进制运算
- 质因数分解
- 线性代数、矩阵运算
- 微积分
- 方程求解
- 逆波兰表达式
- 与 GNUPLOT 对接画图

事实上笔者基本用不到多么高级的功能，主要是加减乘除、幂运算、进制转换等。本文就简要介绍一下基本使用逻辑。

输入命令 `M-x` `calc` 启动计算器。 按 `h` `T` 可以打开教程。

默认使用 RPN（Reverse Polish Notation，逆波兰表达式或称后缀表达式）。所谓 RPN 就是数字会以栈的形式依次放入，在输入运算符时数字出栈、计算、再把结果入栈。如果读者从未听说过后缀表达式，甚至连栈也没有听说过，那么直接只用普通的算数表达式就好。先输入单引号 `'`，然后就可以在 Minibuffer 中输入正常的表达式了。至于逆波兰表达式的学习，读者可以参考《数据结构》课程。

如果是使用 RPN，那么例如计算 $ 2 + 3 \times 4 $ 时需要依次输入：`2`、`3`、`4`、`*`、`+`。 Emacs 会维护一个栈供用户查看，如下图左侧，从下到上依次是栈底到栈顶。右侧可简单理解为历史记录。

![clac](../../images/emacs-book/tricks/calc.png)

特别的，按 `<backspace>` 可以移除栈顶元素，按 `<TAB>` 可以交换栈顶的两个元素，按 `M-<TAB>` 可以把第三个元素拿出来重新放入栈顶，`U` 可以撤销操作。很多运算符也都有含义，例如 `&` 取倒数 ，`^` 取幂等。

人们的第一反应一般是觉得 RPN 非常的反常识，肯定不好用，但事实上，RPN 作为 `calc` 的默认模式还是有一定道理的，至少从笔者日常的体验来说，适应之后似乎确实更省事，毕竟也可以交换栈顶元素，每次可以只考虑局部的运算，然后一步步算出最终结果。普通的算式的话，则需要来回加小括号，或者把结果保存到临时变量中去以备使用。

想要输入十六进制的数，例如 `0xFE`，只需要输入 `16#fe` ，其它进制也一样，N 进制就输入 `N#` 开头，后接数字即可。 N 的取值是 2～36（笔者推测之所以止步于三十六进制是因为 10 个数字+26 个字母共计 36 个字符）。

运算结果默认显示为十进制，想要显示为 N 进制，输入 `d` `r` N。 此外，还有快捷输入：

- 显示为十进制：`d` `0`
- 显示为二进制：`d` `2`
- 显示为十六进制：`d` `6`
- 显示为八进制：`d` `8`

##  Emacs server

由于 Emacs 的启动较慢，在命令行使用 Emacs 时，频繁启动关闭非常不方便。一种方式是启动后按 `C-z` 挂起 Emacs 回到 Shell，之后再输入命令 `fg` （Foreground 的缩写）回到 Emacs。

这里介绍另一种方式。Emacs 为用户提供了一个 client-server 模式，用户可以设置开机启动 Emacs server 的守护进程，然后在需要使用 Emacs 编辑文件时，通过 Emacs client 打开 Emacs。由于 Emacs 已在后台启动，client 可以秒开 Emacs。

手动启动，在 Shell 中输入：

```bash
$ emacs --daemon
```

Emacs 就会在后台加载好，称为一个守护进程。

之后想要打开文件 `test.c` 时，输入：

```elisp
$ emacsclient test.c
```

如果希望开机自动启动 Emacs 守护进程，需要修改操作系统的启动项。Linux 上可使用 `systemd`，macOS 使用 `launchctl`，
Windows 也可支持。读者直接参照 Wiki 中的 “[Adding To OS Startup](https://wikemacs.org/wiki/Emacs_server#Adding_To_OS_Startup)” 小节就好。

图形界面的话，一般没必要使用这一功能，直接打开程序后不关闭就好了。

## ChatGPT

[chatgpt-shell](https://github.com/xenodium/chatgpt-shell) 可以在 Emacs 内通过交互式界面与 ChatGPT 通信。其使用方式非常简单，只需要根据其主页导入插件并设置 OpenAI key：

```elisp
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      ("abcdefg")))))
```

然而这里 **直接把密钥写进配置文件并不是一个好选择** ，因为大家通常会在云端备份自己的 Emacs 配置，会暴露自己的密钥。所以更推荐使用密码管理器，例如 Unix 平台上可以使用 [pass](https://www.passwordstore.org/)，将密钥保存到
`pass` 中并进行这样的配置：

```elisp
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))
```

对于 macOS 来说，则可以利用自带的钥匙串。首先打开终端输入如下命令，其中把 "abcdefg" 换成你自己的 OpenAI Key：

```bash
$ security add-generic-password -a "openai key" -w "abcdefg" -s "api.openai.com"
```

然后定义一个读取钥匙串的辅助函数并进行配置：

```elisp
(when *is-a-mac*
  (defun osx-get-keychain-password (account-name)
	"Gets ACCOUNT-NAME keychain password from OS X Keychain."
	(let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
	  (let ((passwd (shell-command-to-string cmd)))
		(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
		  (match-string 1 passwd))))))
(when *is-a-mac*
  (use-package chatgpt-shell
	:custom
	((chatgpt-shell-openai-key
          (lambda ()
            ;; Here the openai-key should be the proxy service key.
	    (osx-get-keychain-password "openai key"))))))
```

这里引用它的主页的动图展示一下效果。

![clac](../../images/emacs-book/tricks/chatgpt-shell-demo.gif)

## google-this

[主页](https://github.com/Malabarba/emacs-google-this)

在 Emacs 内选中一段文本，按下 `C-c / t` 触发 `google-this`，即可搜索这段文本。注意这个插件一般是在本地机器才会使用，不适合在服务器上用。

```elisp
(use-package google-this
  :ensure t
  :init
  (google-this-mode)) 
```
