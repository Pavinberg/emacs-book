---
title: "入门介绍"
weight: 1
---

## 为什么学习 Emacs

Emacs 是一个文本编辑器系列，包含有多个分支，其中最主流的一支是 [GNU Emacs](https://www.gnu.org/software/emacs/)，大多数情况下所说的 Emacs 都是指 GNU Emacs，本教程也使用 Emacs 指代 GNU Emacs。Emacs 这一名字最早来源于 “Editor MACroS”，后来也有人称它集合了五个主要功能键的首字母 Esc、Meta、Alt、Ctrl、Shift。

Emacs 与 Vi 共同被称为最古老的 Unix 编辑器，一代代程序员对 Emacs 和 Vi （尤其后来出现的 Vim）产生了无休止的争论。Emacs 诞生已有近五十年时间，是世界上最古老而依然活跃（截止 2021 年）的开源软件之一。

Emacs 的主要思路是大量依赖组合快捷键实现高效编辑，这直接导致了想要流畅使用 Emacs 必须要记忆 Emacs 的大量快捷键，需要相当一段时间熟悉。此外，Emacs 编辑器本身所使用的编程语言是 Emacs Lisp 语言，Lisp 语言的方言之一。Lisp 语言是诞生于 1958 年的世上第二古老高级程序设计语言，其语言以“列表”（List）作为语法和核心数据结构，由于其具备强大的宏系统，可以创造各式方言，Emacs Lisp 就是其中之一。这个语言相对于我们常见的编程语言来说，晦涩难懂，同样增加了 Emacs 的学习难度。网上流传着一个有趣的形容各个编辑器学习曲线的图片：

![emacs-learning-curve](../../images/emacs-book/intro/learningCurve.jpg)

那么，既然如此，为什么要学习这个难以学习的编辑器呢？就为了高效+全面+高度定制化。

- 高效：Emacs 将快捷键使用到极致，凡事都尽可能使用快捷键来解决。这样一来，程序员需要记忆繁多的快捷键，但一旦形成了肌肉记忆，将能得到极高的效率。笔者根据自己的体验，在熟练使用 Emacs 的快捷键后，使用任何常规编辑器都会觉得效率低下。即使是使用如 Visual Studio Code 或 JetBrains IDE 系列辅以 Emacs 键位，由于其功能不全，体验也很差。
- 全面：除了基本的编辑功能，可以与其它各种工具相结合，完成编译、调试、版本管理等各类开发任务，可以说如果你想，可以将它打造为一个功能强大的 IDE。
- 可扩展与定制化：事实上极强的可扩展性才是我认为的 Emacs 最核心的竞争力，前面两个优势归根结底也都是可扩展性带来的优点。与其它号称高可扩展性的编辑器相比，Emacs 内核几乎没有因为安全因素等考虑对用户产生任何限制，因此没有什么是在 Emacs 中扩展不出来的。网络上有着非常大量的 Emacs 插件可供自由搭配选择，你很难想到一个需求是别人没有解决过的，更有大量的功能是你不接触 Emacs 可能永远想不到的。你可以根据你的需求安装插件，而不必安装你不需要的功能，最终你的 Emacs 会是为你自己高度定制化的一个编辑器。Emacs 插件涉及的功能非常广泛，一种调侃 Emacs 的说法是，Emacs 不是一个编辑器，而是一个操作系统，其中甚至有一些游戏、还能让 Emacs 成为一个功能完整的浏览器。下图是一个 xkcd 网站上的漫画 Real Programmers，调侃了 Emacs 可以让蝴蝶扇动翅膀，利用蝴蝶效应引发内存上的一个比特翻转，由此来进行编程，表现了 Emacs 功能的丰富。此外，你可以使用 Emacs Lisp 进行编程，实现你独特的需求。

![real-programmers](../../images/emacs-book/intro/realProgrammers.png)

特别的，在 Linux/macOS 系统的终端中，是可以使用部分 Emacs 快捷键的；在 macOS 系统中的各种文本编辑框，如备忘录、浏览器搜索框等等也都支持 Emacs 光标移动快捷键。因此可以说，掌握 Emacs 快捷键，在各种操作系统中都可以享受到其带来的方便快捷。

## Emacs 的主要相关资料

[Emacs 主页](https://www.gnu.org/software/emacs/)：可在此下载安装包，还有文档、Wiki 等的入口。

[Emacs 官方文档](https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html)：最准确、最全面。有能力可以直接按照官方文档进行学习。

[Emacs Wiki](https://www.emacswiki.org/)：关于 Emacs 插件、Emacs Lisp 的一些使用方法。

[Emacs Lisp 教程](https://www.gnu.org/software/emacs/manual/html_node/eintr/)：Emacs Lisp 的完整教程。

[Emacs StackExchange 问答论坛](https://emacs.stackexchange.com/)：Emacs 的 StackExchange，有着大量关于 Emacs 的提问和精品回答。

[Emacs Reference Card](https://www.gnu.org/software/emacs/refcards/index.html)：即 Emacs 备忘表（Cheatsheet）。前两个 PDF 是基本内容，[基本操作](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)卡片和“[生存卡片](https://www.gnu.org/software/emacs/refcards/pdf/survival.pdf)”。

## 对 Vim 用户

Emacs 有一个 [evil](https://github.com/emacs-evil/evil) 插件可以在 Emacs 中直接使用 Vim 的快捷键，这对 Vim 转 Emacs 用户而言非常友好，降低了转换成本。所以我们常能在社区看到许多 Vim 转 Emacs 的用户。此外，对于不想花过多时间配置的读者，可以使用下一小节中提到的 “Emacs 发行版”。

## 其它 Emacs

本教程所讨论的 GNU Emacs 是最原始的 Emacs，需要从 0 开始配置。部分用户会觉得这样过于枯燥，于是有一些 “Emacs 发行版”，预装了很多插件。这样的 Emacs 有两个：[Doom Emacs](https://github.com/hlissner/doom-emacs) 和 [Spacemacs](https://www.spacemacs.org/)，它们尤其对 Vim 转到 Emacs 的用户比较友好，因为它们预装了 [evil](https://github.com/emacs-evil/evil) 插件，可以在 Emacs 上使用 Vi 的操作。但本文只讲解原生 Emacs，也被称为 Vanilla Emacs。

此外还有一个专门适配 macOS 的 [Aquamacs](https://aquamacs.org/)，对 macOS 的一些特点进行了适配，例如用 Cmd+O 来替代 C-x C-f 打开文件。但笔者更偏爱 GNU Emacs，毕竟它在所有平台上的操作方式都是一致的。

