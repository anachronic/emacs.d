[![Build Status](https://travis-ci.org/anachronic/emacs.d.svg?branch=master)](https://travis-ci.org/anachronic/emacs.d)

# My Emacs config


## A little history.

I started using Emacs when I had to code some C assignments for college. This began around the time I installed Linux for the first time and couldn't find any decent C IDEs around, and trust me, I tried a lot of them. My first introduction to this was from Jerry Cain's lectures at his CS107 course, which I watched online.

I found out years later that Emacs is highly customizable and started playing around. So far I'm able to understand a little elisp and create my own functions, which is why I love Emacs so much.

## Use

Well, this config should work in any linux system, and while it is usable, we all have our own config files and directories. But feel free to use it, clone it and dig around the code inside. Most of the packages are auto-installed when you first run Emacs. But in case you want to know, you'll need to manually install **elfeed** in order to read my RSS feeds.

There are some personal configuration files that these files load. But that's only when they exist so that should be fine.

## Packages

I had a list before, but it has grown to the point that it's kind of unmantainable, so I'll just list the important ones. Of course there's `magit`, `yas`, and all that stuff.

* [Magit](https://magit.vc/) - Git interface
* [company](http://company-mode.github.io/) - Autocompletion
* [Helm](https://github.com/emacs-helm/helm) - Input stuff easily
* [ivy](https://github.com/abo-abo/swiper) - Same as above. I default ivy and use Helm for specific stuff.
* [AUCTeX](https://www.gnu.org/software/auctex/) - LaTeX. Previewing things is nice.
* [Projectile](https://github.com/bbatsov/projectile) - Project managing.
* [expand-region](https://github.com/magnars/expand-region.el) - One of the best packages ever.
* [elfeed](https://github.com/skeeto/elfeed) - News reading. Mainly linux and emacs news.
* [multiple-cursors](https://github.com/magnars/multiple-cursors.el) - Another **GREAT** package. Check it out!
* [Elpy](https://github.com/jorgenschaefer/elpy) - Python programming. I'd rather use anaconda, but it doesn't autoimport stuff.
* [helm-gtags](https://github.com/syohex/emacs-helm-gtags) - GNU Global TAGS interface. It's great.
* [dumb-jump](https://github.com/jacktasia/dumb-jump) - Use grep/ag to jump defs. I use as a fallback when TAGS are not available or don't work as expected.

That's the important stuff, I'm sure you can find better ones than me. Youtube is full of great videos that not only show great packages but also have demos or can show you how to use them. What has made a difference for me is reading people's blogs. Seriously, if you haven't, you should. I have found **amazing** code snippets out there, the most recent one being `narrow-or-widen-dwim`. If you don't know what narrow is, [look it up](https://www.gnu.org/software/emacs/manual/html_node/emacs/Narrowing.html). Most of what I've copied or adapted are in my elfeed feeds list. But I'll include them, because they're truly amazing:

* [or emacs](http://oremacs.com/)
* [emacs redux](http://emacsredux.com/)
* [Endless parentheses](http://endlessparentheses.com/)
* [Mastering Emacs](https://www.masteringemacs.org/all-articles)

There are others, but I haven't checked them out. If you find any interest one, I beg you let me know!


## What is this good for?

My main goal is to make this an editor that's usable for any text editing task I have to face. Currently the config that I feel is ready to go on the fly are the following modes:

1. Python
2. C/C++
3. Javascript/Node.js
4. Web mode (default is Django for now)
5. Markdown, ofc.
6. Vala. I haven't used this yet, but I do intend to.

There are also other *not-so-important* major modes like `gitignore-mode` here. Whatever.

## Future work

Most of what I want to do is navigate quickly, have proper syntax highlighting, to be able to jump around and whatnot. I plan to write some elisp packages in the future, but as of these times I'm not able to, because I don't have a lot of time.

## License
You're free to do **whatever** you want with these files with absolutely no restriction. You can read the `LICENSE` file for more info. There are some files that have some restrictions, though.

I've taken a lot of ideas from other people's files. But have, for the most part, written the code myself.

The big exceptions here are:
* The file `setup-shell.el` under the `elisp` directory. I copied the
  majority of it
  from
  [this](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org) file,
  which is under Creative Commons License. I do want to thank Howard,
  because I have learned so much from his files, and that process has
  absolutely helped me understand more about Emacs.
* The `snippets` directory, which i got from [here](https://github.com/AndreaCrotti/yasnippet-snippets). The license is pretty permissive, but I thought I should mention it since the majority of the snippets come from there.
* The **ibuffer** configurations in `setup-ui.el` for `ibuffer` and `ibuffer-vc` as well as the Travis CI build scripts. I directly copied from [Steve Purcell's](https://github.com/purcell/emacs.d) repository. I got some ideas out if it too, but I mostly wrote the code myself and added my own customizations for those packages. Everything is commented in the files, though.
