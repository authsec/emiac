#!/bin/bash

EMIAC_CONFIG_FOLDER="${EMIAC_HOME}/.emiac"

if [ -f "${EMIAC_CONFIG_FOLDER}/setup" ]
then

cat <<'EOF'
[EMIAC INFO]
    Running custom setup file ...

    Make sure the file has a supported interpreter set in the first line.

    This is typically

    #!/bin/bash

    But can also refer to e.g. a python interpreter like

    #!/usr/bin/python3

[/EMIAC INFO]

EOF
    echo "[EMIAC] Ensuring we can execute requested custom setup"
    chmod 0755 ${EMIAC_CONFIG_FOLDER}/setup

    echo -en "[EMIAC] Running custom setup\n\n\n"
    ${EMIAC_CONFIG_FOLDER}/setup
    
else
cat <<EOF
[EMIAC INFO]
If you want to customize your emiac installation
you can create a 'setup' file in ${EMIAC_CONFIG_FOLDER}
and mount that file or folder into the container.

EOF
cat <<'EOF'
Make sure you mount the file properly and make sure you have 
allowed the container runtime access to that folder e.g.

#> colima start --mount $HOME/research:w --mount $HOME/emiac_config_folder:w
You can then mount the folder with the run command like so:

#> docker run -it --rm -e DISPLAY=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'):0 -v ~/research:/home/emiac/research:rw -v ~/emiac_config_folder/setup:/home/emiac/.emiac/setup:rw authsec/emiac

The script will be executed before 'emacs'.
[/EMIAC INFO]

EOF
# EOF Needs to be kept left, don't indent as the if condition will
# no longer work!!
fi

echo "[EMIAC] Starting emacs ..."
emacs