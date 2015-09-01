# heyefi

heyefi is a Linux system daemon that listens for Eye-Fi SD card and processes the requested file upload requests made by the card. This project is not endorsed by Eye-Fi Inc.

# Installation

heyefi is meant to be run as a system daemon. It currently needs to run as root, to make the file permissions part work with less configuration. (It currently sets the permissions of the files to the same as the upload directory. Let me know if you have ideas on how to do that [without being root](https://github.com/ryantm/heyefi/issues/4)!) It expects a configuration file to be at /etc/heyefi/heyefi.config.

# Distributions

Currently, this package is available through Nix or Nixos.

# Configuration

The configuration file has the following options:

cards: a list of Eye-Fi card credentials. The format of the list is [["macaddress1","uploadkey1"],["macaddress2","uploadkey2"],...]

upload_dir: an string that is an absolute path to where the files get put after they come off the card.

Complete example config with two cards:

````haskell
cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
upload_dir = "/data/unsorted"
````

# NixOS configuration

There is a NixOS service definition for heyefi. It currently only supports one card's credentials.

Complete example configuration:

````nix
{
  services.heyefi.enable = true;
  services.heyefi.cardMacaddress = "0012342de4ce";
  services.heyefi.uploadKey = "e7403a0123402ca062";
  services.heyefi.uploadDir = /data/unsorted;
}
````
