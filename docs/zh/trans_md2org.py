import re

name = "orgmode"
mdfile = f"{name}.md"
orgfile = f"{name}.org"

md2org_patterns = [
    (r'\[([^]]+)\]\(([^)]+)\)', r'[[\2][\1]]'), # URL
    (r'```bash', r'#+begin_src bash'),
    (r'```lisp', r'#+begin_src elisp'),
    (r'```elisp', r'#+begin_src elisp'),
    (r'```c', r'#+begin_src c'),
    (r'```python', r'#+begin_src python'),
    (r'```', r'#+end_src'),
    (r'`(.*?)`', r' ~\1~ '), # inline code
    (r'\s\s', r' ')
]
with open(mdfile) as fp, open(orgfile, 'w') as op:
    _ = fp.readline()
    title = fp.readline().split(maxsplit=2)[-1].strip('"')
    op.write(f"#+TITLE: {title}\n")

    weight = fp.readline().split(maxsplit=2)[-1]
    op.write(f"#+WEIGHT: {weight}\n")

    _ = fp.readline()

    for line in fp:
        nline = line
        img = re.search(r'\!\[(.*?)\]\((.*?)\)', nline)
        if img:
            title = img.group(1)
            url = img.group(2)
            op.write(f'#+NAME: {title}\n[[{url}]]\n')
        else:
            if nline.startswith('#'):
                i = 0
                for c in nline:
                    if c == '#':
                        i += 1
                nline = '*' * i + nline[i:]
            for md, org in md2org_patterns:
                nline = re.sub(md, org, nline)
            op.write(nline)
