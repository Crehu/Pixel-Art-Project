.PHONY: build_sprite build_menu watch

build_sprite:
	elm make --output=sprite.js src/Sprite.elm

build_menu:
	elm make --output=menu.js src/Menu.elm

watch:
	while true; do \
		inotifywait -qr -e modify,create,delete,move .; \
		make build_sprite build_menu; \
	done