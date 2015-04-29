# uinitctl

## Introduction

Uinitd is a userspace init system similar to systemd/User or the typical RC
file called by a window manager. Uinitd is written in Haskell and is completely
window manager agnostic.

## Configuration

Uinitd is configured by a single file. The file location can either be given
on the command line using the `-c` flag, otherwise it first checks for
`~/.config/uinitd.conf` and finally `/etc/uinitd.conf`.

The configuration file requires 5 options to be set.

##### Service Directory

The service directory is where new service files will be placed, and service files
will be read on init.

##### Enabled Services File

This is the file where enabled service configurations are stored. It is not meant to
be user editable and the file will be created on enabling a service if it does not
exist. The directory of the file needs to exist, however.

##### Logfile

Location for the logging file. File does not need to exist, but the directory does.

##### PID Directory

Directory to place the PID of the deamon.

##### Daemon Port

Port that the daemon listens on.

### Sample Config

```
# Example default configuration
services: ~/.uinitd/services
enabled: ~/.uinitd/services.list
logfile: ~/.uinitd/uinitd.log
pid_directory: ~/.uinitd
port: 5000
```

## Usage

The daemon is first started using the command `init`.

`uinitctl init` or `uinitctl init -c myconfig.conf`

To create a new service the command `create` is used and a name and path is given.

`uinitctl create -s background -e "feh --bg-scale background.png"`

This creates a file background.service and places it in the services directory. We can
now use the list command to see that it is an available service.

```
> uinitctl list
Running:
Enabled:
Available:
    [background]: feh --bg-scale background.png
```

If we want to start the service, use the `start` command with the name of the service.

```
> uinitctl start -s background
> uinitctl list
Running:
    [background]: feh --bg-scale background.png
Enabled:
Available:
    [background]: feh --bg-scale background.png
```

Likewise, it is stopped with the `stop` command, or restarted with the `restart` command.

```
> uinitctl stop -s background
> uinitctl list
Running:
Enabled:
Available:
    [background]: feh --bg-scale background.png
```

A service that is enabled will be run when `uinitctl init` is called, this is the
primary function of the program.

```
> uinitctl enable -s background
> uinitctl list
Running:
Enabled:
    [background]: feh --bg-scale background.png
Available:
    [background]: feh --bg-scale background.png
```

An enabled service can be disabled with the command `disable` in the same way.

#### Caveats

Service files and the service list are not meant to be user editable. They use a fairly
simple syntax, but there are no garauntees that user edits will not be destroyed. All
changes should be done using the built in commands.
