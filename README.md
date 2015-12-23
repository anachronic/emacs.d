# Yet Another Emacs config directory.


### So why Emacs?
So, i started using Emacs not so long ago, because i could never find
a reasonable editor for C coding. I was actually using Clion last
time, and before that I was using Emacs out of the box. Overall, I
think JetBrains is a really cool company, but Clion just wasn't right
for me. I mean, syntax highlighting is terrible, there's absolutely no
autoinclude functionality, autocomplete works only over project
defined structures as opposed to files in include directories.

So I started searching for something that could make me as productive
in C as IntelliJ makes me when I write Java code, and since i knew
emacs was better than Clion because, well, it has better syntax
highlighting out of the box, i started looking for a way to have
autocomplete on Emacs.

And there started my journey over this awesome *Operating System*.  It
didn't take me too long to figure out that C is not the only thing
Emacs can do, so there's a lot more i'll be doing in this repository,
because I want it to be my editor for everything.

### Flaws

I do not recommend using this config anyway, because i just started
writing it, and it doesn't autoinstall packages, so it can be a little
cryptic to get this config to work, but it is possible, nonetheless.

### So what's in it?

The following:

* Magit
* Company
* company-c-headers
* company-jedi
* company-statistics
* company-quickhelp
* Evil Nerd Commenter
* Helm
* Projectile
* helm-projectile
* helm-gtags
* discover-my-major
* Markdown Mode
* iedit
* NeoTree
* PDF Tools
* Powerline
* py-autopep8
* pydoc-info
* smex
* shell-pop
* YASnippet

And a bunch of other standalone scripts collected from Stack Overflow
and EmacsWiki. I do plan on getting more things into it like Ruby
support, Django support, and writing a header autoinclude for plain C.

This config also requires some tools that don't come from MELPA, like
pylint, or git, and something. I will write about this at some point.

Last, but not least, I got a bunch of snippets for YASnippet from some
repository (I think its AndreaCrotti's) and added my own.
