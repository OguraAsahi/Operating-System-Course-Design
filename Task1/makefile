all:
	nasm pmtest9.asm -o pmtest9.com

	sudo mount -o loop pm.img /mnt/floppy/

	sudo cp pmtest9.com  /mnt/floppy/ -v

	sudo umount /mnt/floppy/
