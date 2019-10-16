import { Sprite, Texture } from 'pixi.js';

export default class Cell extends Sprite {
  /**
   * Create cell
   * extends PIXI.Sprite
   *
   * @param {number}    owner_id    Id of owner of cell
   * @param {number}    x           x coordinate of cell
   * @param {number}    y           y coordinate of cell
   * @param {String}    color       color assigned to cell
   *
   * @return {Cell}
   */
  constructor(ownerId, x, y, color) {
    super(Texture.WHITE);
    this.owner = ownerId;
    this.cellx = x;
    this.celly = y;
    this.tint = color;
    this.width = this.height = 10;
    this.position.set(x * 10, -y * 10);
    this.interactive = true;
  }
}
