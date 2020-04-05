import { Container } from 'pixi.js';

export default class GameFrame extends Container {
  /**
   * Create GameFrame
   * (extends PIXI.Container)
   * @constructor
   * @param   {Frame}     frame           Starting frame to render
   * @param   {Number}    [width=1000]    Width for PIXI.Container
   * @param   {Number}    [height=1000]   Width for PIXI.Container
   *
   * @return  {GameFrame}
   */
  constructor(frame, width = 1000, height = 1000) {
    super({
      width,
      height,
    });
    this.turnid = frame[0].turnid;
    this.addCells(frame);
  }

  /**
   * Clears current game frame
   */
  clear() {
    this.removeChildren();
    document.querySelectorAll('.cellInfoContainer').forEach(c => {
      c.parentNode.removeChild(c);
    });
  }

  /**
   * Add cells to game frame
   * @param   {Frame}   frame   Frame with cells to add
   */
  async addCells(frame) {
    await new Promise((resolve, reject) => {
      try {
        frame.forEach(player => {
          player.cells.forEach(cell => {
            this.addChild(cell);
          });
        });
        resolve();
      } catch (e) {
        reject(e);
      }
    });
  }

  /**
   * Mounts the current frame to be rendered by GameFrame
   * @example
   * gf.mountFrame(newframe);
   */
  mountFrame(frame) {
    this.clear();
    this.addCells(frame);
  }
}
