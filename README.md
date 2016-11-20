# .emacs.d
My Emacs configuration.

<img src="https://github.com/eccentricayman/.emacs.d/blob/master/Screenshots.png"></img>

<img src="https://github.com/eccentricayman/.emacs.d/blob/master/Screenshots/emacs-term.png"></img>

## Custom Commands

C-Tab: Cycle through windows (GUI)

C-c f: Open NeoTree

C-c g: Open magit

M-S-up: Shift current line up (GUI)

M-S-up: Shift current line down (GUI)

C-c P: Run Processing sketch

C-c j: Compile and run java file

C-c p: Run python file

C-c s: Run shell script

C-c c: Compile and run C file

C-x SPC: Start avy

## More Info
Scrollbar, menubar and the such are disabled.

Default tab width is 4, not 2.

All backups are saved in a ~/.emacsbackups directory.

Having srgb support means better colors, but messed up modeline colors. To fix the modeline colors in exchange for worse color accuracy overall, uncomment line 124.

Built for OSX.

## TODO:
 - Clean up packages
 - Start using use-package and init.el instead of .emacs
 - Move the emacs backups folder inside .emacs.d and add a function to open it
 - Somehow fix the spaceline srgb issue...
