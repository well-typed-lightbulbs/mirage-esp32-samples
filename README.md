# Mirage unikernel samples for ESP32 chips!

## What

### ap_dhcp

This unikernel creates a wifi access point on which up to 4 devices can connect (ESP32 driver limit). 
It embeds the charrua DHCP server to allocate IPs to stations.

### lcd_wifi_demo

This unikernel does a lot of things and is designed for ESP WROVER KIT boards that has an LCD display.
- connects to a specified wifi access point.
- gets an IP via DHCP.
- download a bitmap picture from an ip/port.
- display the picture on the LCD.
- hosts a TCP server that listens for order to move the picture on the screen.

## How

This remains experimental but I've done a lot to make the process as simple as possible.

* Add my custom opam repository for esp32 packages: 
`opam repo add esp32-git https://github.com/TheLortex/opam-cross-esp32.git`
* Install mirage configuration tool (or update it to 3.1.0+dev):
`opam install mirage`
* Go in the unikernel directory that you want to run and type the following commands:
```
mirage config -t esp32
make depends
mirage build
make flash monitor
```

You can use `make menuconfig` after the first configuration step to set up ESP32 settings, 
such as serial port, flash speed and size, support for external ram 
(mandatory for more complex unikernels as they still consume a lot of memory).

## Why

I feel like people can start to test my stuff as I've been able to sort everything into packages.
However the main problem remains that a Mirage unikernel barely holds onto a simple ESP32 board 
and often needs an external SPI RAM as the application uses an advanced network stack for example.
To test these examples I recommend you to have a wrover esp32 module (the ones that has 4MB of additional RAM). 
