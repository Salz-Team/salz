import Player from '../entities/player';

export default class Frame extends Array {
  constructor(frame) {
    super();

    frame.forEach((player) => {
      const p = new Player(player.playerid, player.color, player.pos);
      this.push(p);
    });
  }
}
