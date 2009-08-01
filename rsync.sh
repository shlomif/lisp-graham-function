#!/bin/bash
rm -fr Exported
svn export . Exported
rsync -r -v --progress --rsh=ssh Exported ${__HOMEPAGE_REMOTE_PATH}/Files/files/code/lisp/graham-function/

