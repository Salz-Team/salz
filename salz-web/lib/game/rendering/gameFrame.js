import { Container } from 'pixi.js';

export default class GameFrame extends Container {
  /**
   * Create GameFrame
   * (extends PIXI.Container)
   * @constructor
   * @param   {Frame}     frame           Starting frame to render
   * @param   {number}    [width=1000]    Width for PIXI.Container
   * @param   {number}    [height=1000]   Width for PIXI.Container
   *
   * @return  {GameFrame}
   */
  constructor(frame, width = 1000, height = 1000) {
    super({
      width,
      height
    });
    this.frame = frame;

    this.addCells();
  }

  /**
   * Clears current game frame
   */
  clear() {
    this.removeChildren();
    document.querySelectorAll('.cellInfoContainer').forEach((c) => {
      c.parentNode.removeChild(c);
    });
  }

  addCells() {
    this.frame.players.forEach((player) => {
      player.cells.forEach((cell) => {
        this.addChild(cell);
      });
    });
  }

  /**
   * Mounts the current frame to be rendered by GameFrame
   * @example
   * gf.mountFrame(newframe);
   */
  mountFrame(frame) {
    this.clear();
    this.frame = frame;
    this.addCells();
  }
}
