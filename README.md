## GNU Emacs / N Λ N O Modeline

A simple modeline. Best with [N Λ N O theme](https://github.com/rougier/nano-theme).

![](images/nano-modeline.png)

<div>
<img src="./images/nano-modeline-light.png" width=47.5%>
<img src="./images/nano-modeline-dark.png"  width=47.5%>
</div>


### Installation

Install with [straight.el](https://github.com/raxod502/straight.el):

```
;; Optional (N Λ N O theme)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-modeline"))
```

### Usage

```
(nano-modeline)
```
