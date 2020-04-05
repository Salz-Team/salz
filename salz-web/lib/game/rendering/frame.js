import Player from '../entities/player';

/**
 * @class Frame
 * @extends Array
 *
 * @description A game frame is an array of {@link Player}
 */
export default class Frame extends Array {
  /**
   * @constructor
   * @param   {Array}   frame   Frame with cells to add
   *
   * @return {Frame}
   */
  constructor(frame) {
    super();
    this.turnid = frame[0].turnid;
    this.setupPlayers(frame);
  }

  /**
   * Add players to frame
   * @param   {Array}   frame   Frame with cells to add
   */
  async setupPlayers(frame) {
    await new Promise((resolve, reject) => {
      try {
        frame.forEach(player => {
          const p = new Player(player.playerid, player.color, player.pos);
          this.push(p);
        });
        resolve();
      } catch (e) {
        reject(e);
      }
    });
  }
}
