import * as PIXI from 'pixi.js';

export default class Frame {
  constructor(viewport, frame) {
    this.viewport = viewport;
    this.frame = frame;
  }

  draw() {
    this.frame.forEach((player) => {
      this.drawCells(player.coords, player.color);
    });
  }

  drawCells(coords, color) {
    coords.forEach((coord) => {
      // $this.drawCell(coord, color);
      const sprite = this.viewport.addChild(
        new PIXI.Sprite(PIXI.Texture.WHITE)
      );
      sprite.tint = color;
      sprite.width = sprite.height = 10;
      sprite.position.set(coord.x * 10, coord.y * 10);
    });
  }

  drawCell(coord, color) {
    const sprite = this.viewport.addChild(new PIXI.Sprite(PIXI.Texture.WHITE));
    sprite.tint = color;
    sprite.width = sprite.height = 10;
    sprite.position.set(coord.x * 10, coord.y * 10);
  }
}
