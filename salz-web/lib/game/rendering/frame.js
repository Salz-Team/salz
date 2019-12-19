import Player from '../entities/player';

export default class Frame extends Array {
  /**
   * Create Frame
   * (extends Array)
   *
   * Add players to frame
   *
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
        frame.forEach((player) => {
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
