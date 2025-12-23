# eglot-jdtls-go-source
Go to Java source code for emacs eglot+jdtls Java development.

`eglot-jdtls-go-source` is an Emacs package that enable user to go to the Java source code of Java libraries using `xref-find-definition`, for example: the source code of JDK and the source code of Maven libraries.

## Installation

Git clone the repository
``` bash
  git clone https://github.com/huangfeiyu/eglot-jdtls-go-source.git
```

## Usage
### Enable eldoc-mouse:
Add the following in your Emacs configuration:

``` elisp

(use-package eglot-jdtls-go-source
  :load-path "/path/to/eglot-jdtls-go-source/"
  :init (setq eglot-extend-to-xref t)
  :after (eglot)
  :config (eglot-jdtls-go-source-enable))
```
Note: `:extendedClientCapabilities (:classFileContentsSupport t)` is needed to be configured to initializationOptions, for example:
``` elisp
   (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs
               `(java-ts-mode . ("/path/to/jdtls" :initializationOptions
                                 (
                                  :extendedClientCapabilities (:classFileContentsSupport t)
                                  )))))
```
## Requirements

    Emacs 27.1 or higher
    eglot version 1.8 or higher

## License

This package is licensed under the GNU General Public License v3 (GPL-3.0-or-later). See the LICENSE file for details.
Contributing

## Contribution
Feel free to open issues and pull requests for improvements. If you encounter any bugs or have feature requests, please create an issue on the GitHub Issues page.

## Acknowledgments
Most of the code are adapted from eglot-java, I make it a package so it is more visible to people who are figuring out how to configure eglot+jdtls to be able to go to Java source code of libraries. 

Thanks to: 
       [dannyfreeman](https://github.com/dannyfreeman)
       [husainaloos](https://github.com/husainaloos)
       [yveszoundi](https://github.com/yveszoundi)
       They worked together to make original code. see also https://github.com/yveszoundi/eglot-java/issues/6

Thanks to [zsxh](https://github.com/zsxh), they help me successfully configure my eglot+jdtls to go to source code.

Author

Huang Feiyu sibadake1@163.com

[melpa-link]: https://melpa.org/#/eldoc-mouse
[melpa-badge]: https://melpa.org/packages/eldoc-mouse-badge.svg
