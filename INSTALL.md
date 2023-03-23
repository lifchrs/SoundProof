After following the instructions below, run ``make run`` in the root directory of the project to run the program.

# Installing OCaml


Here's what we're going to install:

- A Unix development environment
- OPAM, the OCaml Package Manager
- An OPAM *switch* with the OCaml compiler and some packages
- The Visual Studio Code editor, with OCaml support

The installation process will rely heavily on the *terminal*, or text interface
to your computer.  If you're not too familiar with it, you might want to
brush up with a [terminal tutorial][terminal-tutorial].

[terminal-tutorial]: https://ubuntu.com/tutorials/command-line-for-beginners

```{tip}
If this is your first time installing development software, it's worth pointing
out that "close doesn't count": trying to proceed past an error usually just
leads to worse errors, and sadness. That's because we're installing a kind of
tower of software, with each level of the tower building on the previous. If
you're not building on a solid foundation, the whole thing might collapse. The
good news is that if you do get an error, you're probably not alone. A quick
google search will often turn up solutions that others have discovered. Of
course, do think critically about suggestions made by random strangers on the
internet.
```

Let's get started!

## Unix Development Environment

```{important}
**First, upgrade your OS.** If you've been intending to make any major OS
upgrades, do them now. Otherwise when you do get around to upgrading, you might
have to repeat some or all of this installation process. Better to get it out of
the way beforehand.
```

### Linux

If you're already running Linux, you're done with this step. Proceed to
[Install OPAM](install-opam), below.

### Mac

Beneath the surface, macOS is already a Unix-based OS. But you're going to need
some developer tools and a Unix package manager. There are two to pick from:
[Homebrew][homebrew] and [MacPorts][macports]. From the perspective of this
textbook and CS 3110, it doesn't matter which you choose:

- If you're already accustomed to one, feel free to keep using it. Make sure to
  run its update command before continuing with these instructions.

- Otherwise, pick one and follow the installation instructions on its website.
  The installation process for Homebrew is typically easier and faster, which
  might nudge you in that direction. If you do choose MacPorts, make sure to
  follow *all* the detailed instructions on its page, including XCode and an X11
  server. **Do not install both Homebrew and MacPorts**; they aren't meant to
  co-exist. If you change your mind later, make sure to uninstall one before
  installing the other.

After you've finished installing/updating either Homebrew or MacPorts, proceed
to [Install OPAM](install-opam), below.

[homebrew]: https://brew.sh/
[macports]: https://www.macports.org/install.php

### Windows

Unix development in Windows is made possible by the Windows Subsystem for Linux
(WSL). If you have a recent version of Windows (build 20262, released November
2020, or newer), WSL is easy to install. If you don't have that recent of a
version, try running Windows Update to get it.

```{tip}
If you get an error about the "virtual machine" while installing WSL, you might
need to enable virtualization in your machine's BIOS. The instructions for that
are dependent on the manufacturer of your machine. Try googling "enable
virtualization [manufacturer] [model]", substituting for the manufacturer and
model of your machine. This [Red Hat Linux][rh-virt] page might also help.
```

**With a recent version of Windows,** and assuming you've never installed WSL
before, here's all you have to do:

- Open Windows PowerShell as Administrator. To do that, click Start, type
  PowerShell, and it should come up as the best match. Click "Run as
  Administrator", and click Yes to allow changes.

- Run `wsl --install`. (Or, if you have already installed WSL but not Ubuntu
  before, then instead run `wsl --install -d Ubuntu`.) When the Ubuntu download
  is completed, it will likely ask you to reboot. Do so. The installation will
  automatically resume after the reboot.

- You will be prompted to create a Unix username and password. You can use any
  username and password you wish. It has no bearing on your Windows username and
  password (though you are free to re-use those). Do not put a space in your
  username. Do not forget your password. You will need it in the future.

```{warning}
*Do not proceed* with these instructions if you were not prompted to create a
Unix username and password. Something has gone wrong. Perhaps your Ubuntu
installation did not complete correctly. Try uninstalling Ubuntu and
reinstalling it through the Windows Start menu.
```

Now skip to the "Ubuntu setup" paragraph below.

**Without a recent version of Windows,** you will need to follow
[Microsoft's manual install instructions][wsl-manual]. WSL2 is preferred over
WSL1 by OCaml (and WSL2 offers performance and functionality improvements), so
install WSL2 if you can.

[wsl-manual]: https://docs.microsoft.com/en-us/windows/wsl/install-manual
[rh-virt]: https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/6/html/virtualization_administration_guide/sect-virtualization-troubleshooting-enabling_intel_vt_and_amd_v_virtualization_hardware_extensions_in_bios

**Ubuntu setup.** These rest of these instructions assume that you installed
Ubuntu (20.04) as the Linux distribution. That is the default distribution in
WSL. In principle other distributions should work, but might require different
commands from this point forward.

Open the Ubuntu app. (It might already be open if you just finished installing
WSL.) You will be at the *Bash prompt*, which looks something like this:

```console
user@machine:~$
```

```{warning}
If that prompt instead looks like `root@...#`, something is wrong. Did you
create a Unix username and password for Ubuntu in the earlier step above? If so,
the username in this prompt should be the username you chose back then, not
`root`. *Do not proceed* with these instructions if your prompt looks like
`root@...#`. Perhaps you could uninstall Ubuntu and reinstall it.
```

Enable copy-and-paste:

- Click on the Ubuntu icon on the top left of the window.
- Click Properties
- Make sure â€œUse Ctrl+Shift+C/V as Copy/Pasteâ€ is checked.

 Now Ctrl+Shift+C will copy and Ctrl+Shift+V will paste into the terminal. Note
 that you have to include Shift as part of that keystroke.

Run the following command to update the *APT package manager*, which is what
helps to install Unix packages:

```console
sudo apt update
```

You will be prompted for the Unix password you chose. The prefix `sudo` means to
run the command as the administrator, aka "super user". In other words, do this
command as super user, hence, "sudo".

```{warning}
Running commands with `sudo` is potentially dangerous and should not be done
lightly. Do not get into the habit of putting `sudo` in front of commands, and
do not randomly try it without reason.
```

Now run this command to upgrade all the APT software packages:

```console
sudo apt upgrade -y
```

Then install some useful packages that we will need:

```console
sudo apt install -y zip unzip build-essential
```

**File Systems.** WSL has its own filesystem that is distinct from the Windows
file system, though there are ways to access each from the other.

- When you launch Ubuntu and get the $ prompt, you are in the WSL file system.
  Your home directory there is named `~`, which is a built-in alias for
  `/home/your_ubuntu_user_name`. You can run `explorer.exe .` (note the dot at
  the end of that) to open your Ubuntu home directory in Windows explorer.

- From Ubuntu, you can access your Windows home directory at the path
  `/mnt/c/Users/your_windows_user_name/`.

- From Windows Explorer, you can access your Ubuntu home directory under the
  Linux icon in the left-hand list (near "This PC" and "Network"), then
  navigating to Ubuntu &rarr; `home` &rarr; `your_ubuntu_user_name`. Or you can
  go there directly by typing into the Windows Explorer path bar:
  `\\wsl$\Ubuntu\home\your_ubuntu_user_name`.

Practice accessing your Ubuntu and Windows home directories now, and make
sure you can recognize which you are in. For advanced information, see
Microsoft's [guide to Windows and Linux file systems][wsl-fs].

We recommend storing your OCaml development work in your Ubuntu home directory,
not your Windows home directory. By implication, Microsoft also recommends that
in the guide just linked.

[wsl-fs]: https://docs.microsoft.com/en-us/windows/wsl/filesystems

(install-opam)=
## Install OPAM

**Linux.** Follow the [instructions for your distribution][opam-install].

**Mac.** If you're using Homebrew, run this command:

```console
brew install opam
```

If you're using MacPorts, run this command:

```console
sudo port install opam
```

[opam-install]: https://opam.ocaml.org/doc/Install.html

**Windows.** Run this command from Ubuntu:

```console
sudo apt install opam
```

## Initialize OPAM

```{warning}
Do not put `sudo` in front of any `opam` commands. That would break your OCaml
installation.
```

**Linux, Mac, and WSL2.**  Run:

```console
opam init --bare -a -y
```

It is expected behavior to get a note about making sure `.profile` is well
sourced in `.bashrc`. You don't need to do anything about that.

**WSL1.** Hopefully you are running WSL2, not WSL1. But on WSL1, run:

```console
opam init --bare -a -y --disable-sandboxing
```

It is necessary to disable sandboxing because of an [issue involving OPAM and
WSL1][bwrap].

[bwrap]: https://github.com/ocaml/opam-repository/issues/12050

## Create an OPAM Switch

A *switch* is a named installation of OCaml with a particular compiler version
and set of packages. You can have many switches and, well, switch between them
&mdash;whence the name. Create a switch for this semester's CS 3110 by running
this command:

```console
opam switch create cs3110-2023sp ocaml-base-compiler.4.14.0
```

```{tip}
If that command fails saying that the 4.14.0 compiler can't be found, you
probably installed OPAM sometime back in the past and now need to update it. Do
so with `opam update`.
```

You might be prompted to run the next command. It won't matter whether you do or
not, because of the very next step we're going to do (i.e., logging out).

```console
eval $(opam env)
```

Now we need to make sure your OCaml environment was configured correctly.
**Logout from your OS (or just reboot).** Then re-open your terminal
and run this command:

```console
opam switch list
```

You should get output like this:

```
#  switch         compiler                    description
â†’  cs3110-2023sp  ocaml-base-compiler.4.14.0  cs3110-2023sp
```

There might be other lines if you happen to have done OCaml development before.
Here's what to check for:

- You **must not** get a warning that "The environment is not in sync with the
  current switch. You should run `eval $(opam env)`". If either of the two
  issues below also occur, you need to resolve this issue first.

- There must be a right arrow in the first column next to the `cs3110-2023sp`
  switch.

- That switch must have the right name and the right compiler version, 4.14.0.

```{warning}
If you do get that warning about `opam env`, something is wrong. Your shell is
probably not running the OPAM configuration commands that `opam init` was meant
to install. You could try `opam init --reinit` to see whether that fixes it.
Also, make sure you really did log out of your OS (or reboot).
```

Continue by installing the OPAM packages we need:

```console
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
```

Make sure to grab that whole line above when you copy it. You will get some
output about editor configuration. Unless you intend to use Emacs or Vim for
OCaml development, you can safely ignore that output. We're going to use VS Code
as the editor in these instructions, so let's ignore it.

You should now be able to launch utop, the OCaml Universal Toplevel.

```console
utop
```

```{tip}
You should see a message "Welcome to utop version ... (using OCaml version
4.14.0)!" If the OCaml version is incorrect, then you probably have an
environment issue. See the tip above about the `opam env` command.
```

Enter 3110 followed by two semi-colons. Press return. The # is the utop prompt;
you do not type it yourself.

```ocaml
# 3110;;
- : int = 3110
```

Stop to appreciate how lovely `3110` is. Then quit utop. Note that this time you
must enter the extra # before the quit directive.

```ocaml
# #quit;;
```

A faster way to quit is to type Control+D.

## Double Check OCaml

If you're having any trouble with your installation, follow these double-check
instructions. Some of them repeat the tips we provided above, but we've put them
all here in one place to help diagnose any issues.

First, **reboot your computer**. We need a clean slate for this double check.

Second, run utop, and make sure it works. If it does not, here are some common
issues:

- **Are you in the right Unix prompt?** On Mac, make sure you are in whatever
  Unix shell is the default for your Terminal: don't run bash or zsh or anything
  else manually to change the shell. On Windows, make sure you are in the Ubuntu
  app, not PowerShell or Cmd.

- **Is the OPAM environment set?** If utop isn't a recognized command, run
  `eval $(opam env)` then try running utop again. If utop now works, your login
  shell is somehow not running the right commands to automatically activate the
  OPAM environment; you shouldn't have to manually activate the environment with
  the `eval` command. Probably something went wrong earlier when you ran the
  `opam init` command. To fix it, follow the "redo" instructions below.

- **Is your switch listed?** Run `opam switch list` and make sure a switch named
  `cs3110-2023sp` is listed, that it has the 4.14.0 compiler, and that it is the
  active switch (which is indicated with an arrow beside it). If that switch is
  present but not active, run `opam switch cs3110-2023sp` then see whether utop
  works. If that switch is not present, follow the "redo" instructions below.

**Redo Instructions:** Remove the OPAM directory by running `rm -r ~/.opam`.
Then go back to the OPAM initialization step in the instructions way above, and
proceed forward. Be extra careful to use the exact OPAM commands given above;
sometimes mistakes occur when parts of them are omitted. Finally, redo the
double check: reboot and see whether utop still works.

```{important}
You want to get to the point where utop immediately works after a reboot,
without having to type any additional commands.
```

