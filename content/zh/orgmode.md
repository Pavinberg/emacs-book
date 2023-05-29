---
title: 笔记系统 org-mode 
weight: 10
---

[org-mode](https://orgmode.org/) 是 Emacs 中非常强大的笔记模式，它的语法与 Markdown 类似，但 Emacs 为其赋予了很多功能，使用体验极佳，例如：

- 章节折叠、跳转
- 方便的多状态、多层级 Todo List
- 丰富多样的链接引用功能
- 表格操作（甚至可以像 Excel 一样输入公式自动计算）
- 插入代码
- 导出成各种格式（Markdown、HTML、LaTeX 等）
- 依附于 Emacs 生态， 有着连贯的使用体验

`org-mode` 的主要缺点是一方面需要一定的额外的学习成本，另一方面只能在 Emacs 中使用，不方便直接分享（不过它可以导出成 Markdown、LaTeX、HTML 等）。

读者可能会在很多地方看到 org-mode 的教程，但它们通常只教基本操作，而不介绍额外插件，更不告诉大家这么强大的工具该用在哪里、怎么用。本文的主要目的就是向大家简要介绍几个 org-mode 区别于其它主流笔记软件的“杀手锏”功能和插件，亦是笔者使用 org-mode 的主要用途。而 org-mode 的基本操作不复杂，已有很多教程，这里不会详细介绍。

## 基础结构

org-mode 的[文档](https://orgmode.org/manual/index.html)非常简短，建议读者大致过一遍，至少看看目录。具体的快捷键在官方文档和各种教程中都有介绍，本文对此不啰嗦讲解，但还是针对结构进行一个非常简短的介绍。

只要打开后缀名为 `.org` 的文件就会自动启动 `org-mode`。 语法和 Markdown 类似，只不过星号（`*`）开头是节，或可以说是标题：

```
* 一级标题
** 二级标题
*** 三级标题
```

通过按 `<TAB>` 和 `S-<TAB>` 可以对小节进行折叠，有利于查看。下面则是一个链接（可以不止是链接外部网页，也可以是本地文件）：

```
[[https://zhihu.com][知乎]]
```

`#+` 开头的是该文件的属性，例如下面定义了 `foo` 属性的值为 `bar`。属性主要用于为 org-mode 提供高级功能。

```
#+foo:bar
```

org-mode 的配置自由度非常高，每个人都有自己的偏好，不复杂但可能会很长。因此建议根据[基本配置](../configurations#基本结构)中的内容将 org-mode 相关配置单独放到一个文件中（例如 `~/.emacs.d/lisp/init-org.el`）。

一般来说我们希望把自己的笔记整理到一起，变量 `org-directory` 表示了用户希望把笔记放到哪里，默认是 `~/org/` 目录，可以先进行如下设置：

```elisp
(setq org-directory (file-truename "~/org/"))
```

## Zettelkasten 笔记法

Zettelkasten 笔记法不是一个 org-mode 概念，而是一个很流行的笔记方式。大概的思想就是说，我们以前组织文件、书籍往往是按照内容组织成树形结构，自上而下有一级一级的文件夹，将知识分门别类归纳其中。但我们在学习的过程中，很难在一开始就把知识整理成体系，而是先零散学习，之后随着知识面的增加逐渐形成体系。因此，相比于自上而下的笔记方式，自下而上的方式更符合人的学习过程。此外，知识本身并不是个树形结构，知识之间有很多复杂的关联，应该形成一个知识网络，树形分门别类的方式限制了知识间的关联。简而言之，树形结构的组织方式更像是一个专业的学科综述，它不适合作为我们日常的笔记方法。

Zettelkasten 笔记法就是一个自下而上的知识网络的方式。我们在学习新事物时，先不要去管它到底应该属于哪个分支，而是记下来再说。而等到我们意识到这部分知识和我们已有的知识的关联时，就通过链接的方式与我们已有的知识网络连接起来，由此我们的知识网络就增加了一个新的节点。通过链接的方式，我们可以逐渐组织我们自己的知识网络，没有任何结构性的限制。

那么在 Emacs 中，[org-roam](https://www.orgroam.com) 就提供了这样的功能。以下是一个基本配置：

```elisp
(use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config
   (org-roam-setup)
   :custom
   (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
   :bind
   (("C-c n f" . org-roam-node-find)
    (:map org-mode-map
          (("C-c n i" . org-roam-node-insert)
           ("C-c n o" . org-id-get-create)
           ("C-c n t" . org-roam-tag-add)
           ("C-c n a" . org-roam-alias-add)
           ("C-c n l" . org-roam-buffer-toggle)))))
```

在 `org-roam` 中，一个文件就是一个节点（node）。我们通过它提供的函数来新建文件，即按 `C-c n f` 调用 `org-roam-node-find` 来新建或打开一个笔记文件。新建时会让我们输入一个标题，这个后面随时可改，`org-roam` 会在我们指定的 `org-roam-directory` 里（这里设定的是 `~/org/roam/` 目录）新建一个带时间戳的文件作为实际存放这个笔记的文件，并在属性中绑定一个唯一的 ID，以防止标题重名。我们并不需要手动去访问这个目录里的文件，只需要用 `C-c n f` 来打开文件即可。`org-roam` 会自动和 `ivy` 或 `helm` 协作，提供补全功能。

打开了一个笔记文件后，可以按 `C-c n i` 调用 `org-roam-node-insert` 来关联另一个节点。此外也可以对文件起别名、添加标签等等。其余功能都可以在文档中找到。对一个链接可以按 `C-c C-o` 调用 `org-open-at-point` 打开。

此外，对于新建的笔记，我们可以自定义它的模板，例如笔者希望能记录每个笔记最后一次修改的时间，这里写了较多的辅助函数（`pv/` 前缀的函数），但是逻辑就是在文件保存时（`before-save`）绑定一个 Hook（`pv/org-set-last-modified`），可以把当前的时间记录下来：

```elisp
(use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config
   (org-roam-setup)
   ;;--------------------------
   ;; Handling file properties for ‘LAST_MODIFIED’
   ;;--------------------------
   (defun pv/org-find-time-file-property (property &optional anywhere)
     "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
     (save-excursion
       (goto-char (point-min))
       (let ((first-heading
              (save-excursion
				(re-search-forward org-outline-regexp-bol nil t))))
         (when (re-search-forward (format "^#\\+%s:" property)
                                  (if anywhere nil first-heading)
                                  t)
           (point)))))

   (defun pv/org-has-time-file-property-p (property &optional anywhere)
     "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
     (when-let ((pos (pv/org-find-time-file-property property anywhere)))
       (save-excursion
         (goto-char pos)
         (if (and (looking-at-p " ")
                  (progn (forward-char)
                         (org-at-timestamp-p 'lax)))
             pos
           -1))))
   (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (pv/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))
   :hook
   (before-save . pv/org-set-last-modified) ; 保存文件时调用
   :custom
   (org-roam-directory (concat org-directory "roam/"))  ; 设置 org-roam 目录
   ;; 自定义默认模板
   (org-roam-capture-templates
    '(("d" "default" plain "%?"
       :if-new
       (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                  "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
       :immediate-finish t)))
   :bind (("C-c n f" . org-roam-node-find)
          (:map org-mode-map
            (("C-c n i" . org-roam-node-insert)
            ("C-c n o" . org-id-get-create)
            ("C-c n t" . org-roam-tag-add)
            ("C-c n a" . org-roam-alias-add)
            ("C-c n l" . org-roam-buffer-toggle)))))
```

笔者这里只有一个默认模板，如果用户设定了多个模板，在新建文件时就会询问用户想要用哪个模板新建，十分方便。

## 任务管理

### 简介

org-mode 另一个用途是可以管理自己的任务清单。在毫无自定义配置时，写起来大概是这样的：

![todo-list](../../images/emacs-book/orgmode/todo-list.png)

相信很多读者会和笔者一开始一样心想：“这不就是一个很普通的 todo-list 嘛，这么简单的事情什么软件不能做？而且组织整理任务清单好麻烦的，没有这个习惯。”

但事实上，任务管理是要搭配 [org-agenda](https://orgmode.org/manual/Agenda-Views.html) 一同使用。实际的使用方式是，用户在固定的一个或几个文件中写自己的任务清单，然后调用 `org-agenda` 命令，它会为我们整理一个漂亮的日程安排。其中可以选择很多种方式展示，例如过滤掉已完成的任务，按 Deadline 顺序显示等等。

此外，每个任务可以不止是有“TODO”和”DONE”两种状态，而是自定义任何其它状态；org-mode 还提供了任务优先级功能。于是，就可以有这样的效果：

![org-agenda](../../images/emacs-book/orgmode/org-agenda.png)

笔者当时截图日期是 5 月 11 日周四的 12:20。可以看到，最开头是最高优先级的、也就是亟需完成的任务。紧接着，是前两天开始的 7 天时间的任务概览。其中，今天的任务会详细显示，还包括了红色的前两天超时未完成的任务，以及每周四的 18:30 预订了一个周期性的组会，处于“MEETING”状态。最下方是详细的普通优先级任务，其中第一条处于“WAITING”状态，因为这是笔者要投稿论文，需要在网站注册，但当时该注册网站尚未启动，所以需要等待它开通。右侧两个冒号间的内容则是各个任务的标签（tag），可用于筛选任务。

可以看到，org-mode 结合 org-agenda 后，不仅仅是一个简单的 todo-list，还是一个日历，还可以具备多状态、多优先级、多标签。具备多状态的好处就是，如果一个任务只有完成和没完成两个状态，那如果我们需要等待别人完成我们才能开始时，就需要自己在脑袋里记着，而现在我们有了“WAITING”状态，就可以显式标记出来。同时它还可以是多层级的，大任务下可以写小任务，有利于拆解复杂的任务。而会议其实不算是一个待办事项，所以还有额外的“MEETING”状态。任务可以被取消，所以还有个“CANCELD”状态。

### 配置

笔者的配置主要来自于[这篇博客](https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html)和[这篇博客](http://doc.norang.ca/org-mode.html)。在 `:init` 中，笔者设置了 `pv/org-agenda-files` 为目录 `~/org/Agenda/`，这表示“该目录下的所有文件都是用于写 todo-list 的，在使用 `org-agenda` 时需要对它们进行读取”。随后新建一个文件 `~/org/Agenda/agenda.org` 用来写自己的 todo-list，调用 `C-c a` `d` 就可以展示类似上图的任务列表视图。

```elisp
(setq org-directory (file-truename "~/org/"))
(setq pv/org-agenda-files `(,(concat org-directory "Agenda/")))

(use-package org
  :init
  (require 'org-indent)
  :config
  (defun pv/init-org-hook ()
	(setq truncate-lines nil)
	(org-toggle-pretty-entities)) ; display LaTeX symbols
  (defun pv/org-skip-subtree-if-priority (priority)
	"Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
	(let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
		nil)))
  (defun pv/org-skip-subtree-if-habit ()
	"Skip an agenda entry if it has a STYLE property equal to \"habit\"."
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
		nil)))
  :hook
  (org-mode . pv/init-org-hook)
  :custom
  (org-hide-leading-stars t "clearer way to display")
  (org-startup-with-inline-images t "always display inline image")
  (org-image-actual-width 600 "set width of image when displaying")
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		   (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "goldenrod1" :weight bold)
		   ("NEXT" :foreground "DodgerBlue1" :weight bold)
		   ("DONE" :foreground "SpringGreen2" :weight bold)
		   ("WAITING" :foreground "LightSalmon1" :weight bold)
		   ("CANCELLED" :foreground "LavenderBlush4" :weight bold)
		   ("MEETING" :foreground "IndianRed1" :weight bold))))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
		   ("WAITING" ("WAITING" . t))
		   (done ("WAITING"))
		   ("TODO" ("WAITING") ("CANCELLED"))
		   ("NEXT" ("WAITING") ("CANCELLED"))
		   ("DONE" ("WAITING") ("CANCELLED")))))
  (org-adapt-indentation t)
  (org-agenda-files pv/org-agenda-files)
  ;; Do not dim blocked tasks
  (org-agenda-dim-blocked-tasks nil)
  ;; compact the block agenda view
  (org-agenda-compact-blocks t)
  (org-agenda-span 7)
  (org-agenda-start-day "-2d")
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-column -86) ; default value auto has issues
  ;; Custom agenda command definitions
  (org-agenda-custom-commands
   (quote (("d" "Daily agenda and all TODOs"
			((tags "PRIORITY=\"A\""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
					(org-agenda-overriding-header "High-priority unfinished tasks:")))
			 (agenda "" ((org-agenda-ndays 1)))
			 (alltodo ""
					  ((org-agenda-skip-function '(or (pv/org-skip-subtree-if-habit)
													  (pv/org-skip-subtree-if-priority ?A)
													  (org-agenda-skip-if nil '(scheduled deadline))))
                       (org-agenda-overriding-header "ALL normal priority tasks:"))))
			((org-agenda-compact-blocks t)))
		   ("p" "Projects"
			((agenda "" nil)
             (tags "REFILE"
				   ((org-agenda-overriding-header "Tasks to Refile")
					(org-tags-match-list-sublevels nil)))
             (tags-todo "-CANCELLED/!"
						((org-agenda-overriding-header "Stuck Projects")
                         (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-CANCELLED/!NEXT"
						((org-agenda-overriding-header (concat "Project Next Tasks"
															   (if bh/hide-scheduled-and-waiting-next-tasks
																   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                         (org-tags-match-list-sublevels t)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))
             (tags "-REFILE/"
				   ((org-agenda-overriding-header "Tasks to Archive")
					(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
					(org-tags-match-list-sublevels nil))))
			nil))))
  :bind
  (("C-c a" . 'org-agenda)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))
```

值得一提的是，对一个 TODO 可以用 `org-deadline` 来指定 deadline，用 `org-schedule` 规划时间。例如：

```
* TODO 学习 Emacs 
  DEADLINE: <2023-05-20 Sat>
```

其中在日历中选择日期时，需要按 `Shift`+方向键。对于重复性任务如每周例会，则手动加上 "+7d" 表示每 7 天一个周期，具体参见[文档](https://orgmode.org/manual/Repeated-tasks.html)：

```
* MEETING 例会
  SCHEDULED: <2023-05-25 Thu 18:30 +7d>
```

### 快速添加任务

但毕竟手动组织 `agenda.org` 也需要一定的思考，如果手头正在忙一些事情，但忽然来了个任务，不想被打断，就可以先快速把任务记下来，回头再整理。这个功能叫 `org-capture`，可将如下配置合并到上面的配置中：

```elisp
;; ...
(setq pv/org-refile-file (concat org-directory "refile.org"))
  
(use-package org
  :custom
  ;; ...
  (org-capture-templates
   (quote (("t" "todo" entry (file pv/org-refile-file)
			"* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("r" "respond" entry (file pv/org-refile-file)
			"* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		   ("n" "note" entry (file pv/org-refile-file)
			"* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("w" "org-protocol" entry (file pv/org-refile-file)
			"* TODO Review %c\n%U\n" :immediate-finish t)
		   ("m" "Meeting" entry (file pv/org-refile-file)
			"* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))
  (org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path 'file)
    ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-cite-global-bibliography pv/org-bibtex-files)
  :bind
  ;; ...
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))
```

当我们使用 `C-c c` 调用 `org-capture` 时，会提示我们想要添加的是什么任务。按 `C-c C-c` 保存到 `~/org/refile.org` 中，也就是临时存储。等有空时，我们可以打开 `~/org/refile.org` 文件，对着一个任务输入 `C-c C-w` 调用 `org-refile`，此时会提示你希望把这个任务插入到哪个文件的哪个项目的位置里。

## 论文笔记

结合以上工具，org-mode 可以对论文进行笔记并关联 BibTeX。主要配置就是要告知 Emacs 我们的 BibTeX 文件在哪里，并设定一个存放论文 PDF 的仓库。这里，笔者配置了 `~/org/References` 为仓库，`~/org/References/reference.bib` 为一个总的 BibTeX。二者都是列表，笔者可以根据需要设置多个仓库和多个 BibTeX 文件。笔者还使用了 [org-ref](https://github.com/jkitchin/org-ref) 插件辅助管理。

```elisp
;; ...
(setq pv/org-bibtex-library `(,(concat org-directory "References/")))
(setq pv/org-bibtex-files `(,(concat org-directory "References/references.bib")))

(use-package org
  :custom
  ;; ...
  (org-cite-global-bibliography pv/org-bibtex-files))

(use-package bibtex-completion
  :custom
  (bibtex-completion-pdf-open-function
   (lambda (fpath)
	 (call-process "open" nil 0 nil fpath))) ; 配置打开 PDF 的方式
  (bibtex-completion-bibliography pv/org-bibtex-files)
  (bibtex-completion-library-path pv/org-bibtex-library))

(use-package org-ref
  :ensure t)
```

笔者的使用方式是这样的：
1. 创建一个总的论文列表（或是一类论文的列表），使用 `C-c n f` 调用 `org-roam-node-find` 命令新建一个名为 `papers` 的笔记节点。
2. 获取论文的 BibTeX（可通过官网或 Google Scholar 等获得，也可手写），以[这篇论文](https://www.usenix.org/conference/fast23/presentation/li-qiang-deployed)为例，获取它的 BibTeX 并粘贴到 `~/org/References/references.bib` 中（花括号内的第一个字段是这个论文的 ID 标识，笔者习惯用 Google Scholar 的方式“作者+发表年份+标题第一个单词”来命名）：

```
@inproceedings {li2023more,
author = {Qiang Li and Qiao Xiang and Yuxin Wang and Haohao Song and Ridi Wen and Wenhui Yao and Yuanyuan Dong and Shuqi Zhao and Shuo Huang and Zhaosheng Zhu and Huayong Wang and Shanyang Liu and Lulu Chen and Zhiwu Wu and Haonan Qiu and Derui Liu and Gexiao Tian and Chao Han and Shaozong Liu and Yaohui Wu and Zicheng Luo and Yuchao Shao and Junping Wu and Zheng Cao and Zhongjie Wu and Jiaji Zhu and Jinbo Wu and Jiwu Shu and Jiesheng Wu},
title = {More Than Capacity: Performance-oriented Evolution of Pangu in Alibaba},
booktitle = {21st USENIX Conference on File and Storage Technologies (FAST 23)},
year = {2023},
isbn = {978-1-939133-32-8},
address = {Santa Clara, CA},
pages = {331--346},
url = {https://www.usenix.org/conference/fast23/presentation/li-qiang-deployed},
publisher = {USENIX Association},
month = feb,
}
```

3. 此时我们的剪贴板中应该刚好是这段 BibTeX（如果不是，就复制一下这段 BibTeX）。回到 `papers` 笔记文件中，输入 `M-x` `org-bibtex-yank` 命令，org-mode 就会为我们把这段 BibTeX 解析好放入我们的笔记中。实际上 `org-mode` 会把这些属性都折叠起来，所以比这里的显示要美观很多。

```
* More Than Capacity: Performance-oriented Evolution of Pangu in Alibaba
:PROPERTIES:
:TITLE:    More Than Capacity: Performance-oriented Evolution of Pangu in Alibaba
:BTYPE:    inproceedings
:CUSTOM_ID: li2023more
:AUTHOR:   Qiang Li and Qiao Xiang and Yuxin Wang and Haohao Song and Ridi Wen and Wenhui Yao and Yuanyuan Dong and Shuqi Zhao and Shuo Huang and Zhaosheng Zhu and Huayong Wang and Shanyang Liu and Lulu Chen and Zhiwu Wu and Haonan Qiu and Derui Liu and Gexiao Tian and Chao Han and Shaozong Liu and Yaohui Wu and Zicheng Luo and Yuchao Shao and Junping Wu and Zheng Cao and Zhongjie Wu and Jiaji Zhu and Jinbo Wu and Jiwu Shu and Jiesheng Wu
:BOOKTITLE: 21st USENIX Conference on File and Storage Technologies (FAST 23)
:YEAR:     2023
:ISBN:     978-1-939133-32-8
:ADDRESS:  Santa Clara, CA
:PAGES:    331--346
:URL:      https://www.usenix.org/conference/fast23/presentation/li-qiang-deployed
:PUBLISHER: USENIX Association
:MONTH:    feb
:END:
```

4. 利用 `org-ref` 绑定论文 PDF。在这个论文的下面输入 `cite:li2023more`，即 “cite:” 和这篇论文的 ID。`org-ref` 会自动识别到我们想要关联这个 BibTeX。随后我们可以单击这行字，或按 `C-c C-o` 调用 `org-open-at-point`。`org-ref` 会为我们弹出一个选择窗口，我们可以按 `a` 选择"add pdf to library"，输入下载好的论文的 PDF 路径。`org-ref` 会将该 PDF 复制到我们设定的仓库中，即 `~/org/References/` 中，并命名为 `li2023more.pdf`。

![org-ref](../../images/emacs-book/orgmode/org-ref.png)

5. 当我们想要阅读这篇论文时，对着刚刚的 `cite:li2023more` 按 `C-c C-o`，按 `p` 即可打开该 PDF。这里笔者配置的打开 PDF 的方式是用 Unix 系统的 `open` 命令打开，即用系统默认 PDF 阅读器打开。如果去掉该行配置，就会用 Emacs 打开。
6. 笔记可以直接写在 `papers` 的这篇论文词条下面，如果笔记很长，也可以利用 `org-roam` 新建一个文件并链接到这里即可。

`org-ref` 还提供了多种方式导入论文，例如拖入 PDF、拖入网页 URL 等。但笔者使用中发现这些方式不是很稳定，不是百分之百好用，而上面介绍的步骤是一定有效的。此外，使用 org-mode 进行论文笔记和 [Zotero](https://www.zotero.org)、[Mendeley](https://www.mendeley.com) 等文献管理软件并不冲突，完全可以组合使用。

{{< tip >}}
管理论文其实有很多种方式，例如 `citar`, `org-roam-bibtex` 等，读者可以根据需要自行探索。
{{< /tip >}}
