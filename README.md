# My Emacs config


## A little history.

I started using Emacs when I had to code some C assignments for college. This began around the time I installed Linux for the first time and couldn't find any decent C IDEs around, and trust me, I tried a lot of them. My first introduction to this was from Jerry Cain's lectures at his CS107 course, which I watched online.

I found out years later that Emacs is highly customizable and started playing around. So far I'm able to understand a little elisp and create my own functions, which is why I love Emacs so much.

## Use

Well, this config should work in any linux system, and while it is usable, we all have our own config files and directories. But feel free to use it, clone it and dig around the code inside.

There are some configuration files *I do not provide*, but they're really not essential to begin using Emacs with this conf, they include:

* RSS Reading stuff
* Mail configuration

You can now see why I don't provide them, and why this is not a big deal. The config should work just fine without those files.

## Packages

They should now autoinstall:

* Magit for Git
* company for autocompletion
* helm for a nice way to input stuff
* recentf (with config and helm...) for finding recently used files
* smex for a **good** fuzzy M-x
* company-quickhelp for documentation
* autopair for smooth and good coding
* YASnippet for faster coding with snippets
* Flycheck for linting
* AUCTeX for latex editing, compiling and previewing
* yaml-mode for .yml files.
* ace-window for window quick navigating
* Markdown-mode for editing files like this one.
* Undo-Tree mainly for the redo action
* Projectile for project managing.
* Caroline theme.
* nlinum-relative for better navigation.
* expand-region for cool selection

I will include some more over time.

## What is this good for?

We all use emacs for different purposes, and that's ok. I have been using this lately for LaTeX and Org-mode. I have used it for C programming and some Python. I wouldn't recommend this for Python though, especially if you're doing something like web programming or some really sophisticated stuff, because *it has flaws*. TeX, C and Org should be fine though.

## Future work

Most of what I plan to do include documentation on shortcuts, getting this to work well with C, C++, Python and maybe Ruby. This could change over time though, but I will update this README as this config directory evolves.
