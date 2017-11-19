# heyefi

heyefi is a Linux system daemon that listens for Eye-Fi SD card and processes the requested file upload requests made by the card. This project is not endorsed by Eye-Fi Inc.

# Installation

heyefi is meant to be run as a system daemon. It currently needs to run as root, to make the file permissions part work with less configuration. (It currently sets the permissions of the files to the same as the upload directory. Let me know if you have ideas on how to do that [without being root](https://github.com/ryantm/heyefi/issues/4)!) It expects a configuration file to be at /etc/heyefi/heyefi.config.

This program is available through Nix, and it can be easily installed on Nixos. It can be installed on any Linux distribution like this:

1. Put a configuration file at /etc/heyefi/heyefi.config (format described in the "Configuration" section below)
2. Install Nix `curl https://nixos.org/nix/install | sh`
3. Install heyefi `nix-env -iA nixpkgs.heyefi`. The server binary should now be on your path as `heyefi`.
4. Find the absolute path of heyefi with `readlink $(which heyefi)` (For example, on one system it was /nix/store/b4m467s0byr1v2xfd7acwpq5x4l2dp8f-heyefi-1.1.0.0/bin/heyefi)
5. Make a systemd service (or whatever equivalent for your system) that runs the server binary as the root user

# Configuration

The configuration file has the following options:

cards: a list of Eye-Fi card credentials. The format of the list is [["macaddress1","uploadkey1"],["macaddress2","uploadkey2"],...]

upload_dir: an string that is an absolute path to where the files get put after they come off the card.

Complete example config with two cards:

```haskell
cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
upload_dir = "/data/unsorted"
```

# NixOS configuration

There is a NixOS service definition for heyefi. It currently only supports one card's credentials.

Complete example configuration:

```nix
{
  services.heyefi.enable = true;
  services.heyefi.cardMacaddress = "0012342de4ce";
  services.heyefi.uploadKey = "e7403a0123402ca062";
  services.heyefi.uploadDir = /data/unsorted;
}
```

# Development

```bash
nix-shell
```


# Changelog

2.0.0.0 2017-11-18 Program output should be immediately written to stdout, instead of being buffered until the program finishes.
1.1.0.0 2017-01-02 fixed build of version 0.13.0.0 of optparse-applicative, which removes Data.Monoid exports
