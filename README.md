# My Emacs config


## A little history.

I started using Emacs when I had to code some C assignments for college. This began around the time I installed Linux for the first time and couldn't find any decent C IDEs around, and trust me, I tried a lot of them. My first introduction to this was from Jerry Cain's lectures at his CS107 course, which I watched online.

I found out years later that Emacs is highly customizable and started playing around. So far I'm able to understand a little elisp and create my own functions, which is why I love Emacs so much.

## Use

Well, this config should work in any linux system, and while it is usable, we all have our own config files and directories. But feel free to use it, clone it and dig around the code inside. Most of the packages are auto-installed when you first run Emacs. But in case you want to know, you'll need to manually install **elfeed** in order to read my RSS feeds.

This config *includes* my personal feeds, which you can delete, or whatever, for now they're on `init.el`, and in retrospective, I should change it. I guess i'll do it sometime.

## Packages

They should now autoinstall:

* [Magit](https://magit.vc/) 
* [company](http://company-mode.github.io/) for autocompletion
* [Helm](https://github.com/emacs-helm/helm) for a nice way to input stuff
* [recentf](https://www.emacswiki.org/emacs/RecentFiles) (with config and helm...) for finding recently used files
* [smex](https://github.com/nonsequitur/smex) for a **good** fuzzy M-x
* [company-quickhelp](https://github.com/expez/company-quickhelp) for documentation
* [autopair](autopair) for smooth and good coding
* [YASnippet](https://github.com/joaotavora/yasnippet) for faster coding with snippets
  * [Snippets](https://github.com/AndreaCrotti/yasnippet-snippets) From [AndreaCrotti](https://github.com/AndreaCrotti)'s repository
* [Flycheck](http://www.flycheck.org/en/latest/) for syntax checking
* [AUCTeX](https://www.gnu.org/software/auctex/) for latex editing, compiling and previewing
* [yaml-mode](yaml-mode) for .yml files.
* [ace-window](https://github.com/abo-abo/ace-window)  for window quick navigating
* [Markdown-mode](http://jblevins.org/projects/markdown-mode/) for editing files like this one and GitHub Flavored Markdown.
* [Undo-Tree](http://www.dr-qubit.org/tags/computing-code-emacs.html) mainly for the redo action
* [Projectile](https://github.com/bbatsov/projectile) for project managing.
* [nlinum-relative](https://github.com/CodeFalling/nlinum-relative) for better navigation.
* [expand-region](https://github.com/magnars/expand-region.el) for increasingly expanding regions.
* [elfeed](https://github.com/skeeto/elfeed) for News reading! :)
* [avy](https://github.com/abo-abo/avy) because I really try to avoid `C-n` and `C-p`
* [smart-comment](https://github.com/paldepind/smart-comment) for commenting lines....

I will include some more over time.

## What is this good for?

We all use emacs for different purposes, and that's ok. I have been using this lately for LaTeX and Org-mode. I have used it for C programming and some Python. I wouldn't recommend this for Python though, especially if you're doing something like web programming or some really sophisticated stuff, because *it has flaws*. TeX, C and Org should be fine though.

## Future work

Most of what I plan to do include documentation on shortcuts, getting this to work well with C, C++, Python and maybe Ruby. This could change over time though, but I will update this README as this config directory evolves.
