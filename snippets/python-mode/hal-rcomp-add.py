# -*- mode: snippet -*-
# name: hal-rcomp-add
# key: hradd
# --
$1 = hal.RemoteComponent('$2', timer=100)
$0
$1.ready()
