## N Λ N O Modeline

Nano modeline is a minor mode for GNU/Emacs that modify the modeline as:

`[ status | name (primary)                               secondary ]`

It can be displayed at the bottom (mode-line) or at the top (header-line)
depending on nano-modeline-position custom setting.

There are two sets of faces (for active and inactive modelines) that
can be customized (M-x: customize-group + nano-modeline)

- `nano-modeline-active`           / `nano-modeline-inactive`
- `nano-modeline-active-name`      / `nano-modeline-inactive-name`
- `nano-modeline-active-primary`   / `nano-modeline-inactive-primary`
- `nano-modeline-active-secondary` / `nano-modeline-inactive-secondary`
- `nano-modeline-active-status-RO` / `nano-modeline-inactive-status-RO`
- `nano-modeline-active-status-RW` / `nano-modeline-inactive-status-RW`
- `nano-modeline-active-status-**` / `nano-modeline-inactive-status-**`

### Installation

Install with `M-: (package-install 'nano-modeline)`

### Usage example:

Activate with ` M-x: nano-modeline-mode`

### Configure column and line numbers

Emacs comes with `column-number-mode` off by default.  
If you would like to see the column number, turn it on with `M-x: column-number-mode`  
Similarily, if you do not want to see the line number, turn it off with `M-x: line-number-mode`

### Screenshots (using [N Λ N O theme](https://github.com/rougier/nano-theme)):

![](images/nano-modeline.png)
![](images/nano-modeline-light.png)
![](images/nano-modeline-dark.png)

![](images/nano-modeline-RW-focused.png)
![](images/nano-modeline-RW-unfocused.png)
![](images/nano-modeline-RO-focused.png)
![](images/nano-modeline-RO-unfocused.png)
![](images/nano-modeline-MD-focused.png)
![](images/nano-modeline-MD-unfocused.png)
![](images/nano-modeline-mail.png)
![](images/nano-modeline-term.png)
![](images/nano-modeline-elfeed.png)
![](images/nano-modeline-deft.png)
![](images/nano-modeline-docview.png)
![](images/nano-modeline-agenda.png)


