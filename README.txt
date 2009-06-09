!!! TARGET ENVIROMENT
 Mac OS X 10.5
 Carbon Emacs

!!! INSTALL
!!!! get dot file
$ git clone git://github.com/mataki/emacsdotfile.git
$ cd emacsdotfile
$ git submodule init
$ git submodule update
$ cd .emacs.d/rinari
$ git submodule init
$ git submodule update

!!!! install gems
# gem install rcodetools

!!!! set dot file as symlink
$ ln -s /path/to/emacsdotfile/.emacs ~/.emacs
$ ln -s /path/to/emacsdotfile/.emacs.d ~/.emacs.d

!!!! start emacs
Let's start Emacs

!!!! lanch anything
push Control and ";" in same time.

!!!! search command
push "d" and "f" in same time, so you can view command list in anything view.

!!! UPDATE
$ cd /path/to/emacsdotfile
if you don't have any diff on your repository.
$ git pull

!!!! UPDATE Submodule
if your repository have diff on git submodule
$ git status (in emacsdotfile directory)
then output this result

  Modified .emacs.d/org-mode
  diff --git a/.emacs.d/org-mode b/.emacs.d/org-mode
  index ef57d7b..d27d3e5 160000
  --- a/.emacs.d/org-mode
  +++ b/.emacs.d/org-mode
  @@ -1 +1 @@
  -Subproject commit ef57d7b90cd4aaf70d6943434f180fd577822702
  +Subproject commit d27d3e5fa6ba2de93e9cc9068cc481a22fb9d5ce

$ cd .emacs.d/org-mode
$ git pull




