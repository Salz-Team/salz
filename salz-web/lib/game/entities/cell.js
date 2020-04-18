import { Sprite, Texture } from 'pixi.js';
import CellInfoBox from '../ui/cellInfoBox';

export default class Cell extends Sprite {
  /**
   * Create cell
   * extends PIXI.Sprite
   * @constructor
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

    this.on('mouseover', event => {
      let cellInfoBox;
      const delay = setTimeout(() => {
        const oriEvent = event.data.originalEvent;
        const cellInfoObj = {
          owner: this.owner,
          x: this.cellx,
          y: this.celly,
        };
        cellInfoBox = new CellInfoBox(oriEvent.clientX, oriEvent.clientY, cellInfoObj);
        document.querySelector('#salz-game-inner-view').appendChild(cellInfoBox);
      }, 200);

      // once we leave the cell, remove cellInfoBox
      // only do this if cellInfoBox was created
      this.on('mouseout', () => {
        clearTimeout(delay);
        if (typeof cellInfoBox !== 'undefined') {
          setTimeout(() => {
            document.querySelector('#salz-game-inner-view').removeChild(cellInfoBox);
          }, 100);
        }

        // This is to ensure not we don't end up with
        // multiple mouseout events that have no relationship
        // with one another
        this.removeListener('mouseout');
      });
    });
  }
}
