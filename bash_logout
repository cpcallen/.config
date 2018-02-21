# ~/.bash_logout				-*- shell-script -*-
#
# Executed by bash(1) when login shell exits.

# when leaving the console clear the screen to increase privacy

case "`tty`" in
    /dev/tty[0-9]) clear
esac

# Debian:
# if [ "$SHLVL" = 1 ]; then
#     [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
# fi
