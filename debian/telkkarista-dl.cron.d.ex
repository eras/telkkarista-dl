#
# Regular cron jobs for the telkkarista-dl package
#
0 4	* * *	root	[ -x /usr/bin/telkkarista-dl_maintenance ] && /usr/bin/telkkarista-dl_maintenance
