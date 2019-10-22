import Player from '../entities/player';

export default class Frame {
  constructor(frame) {
    this.players = [];

    frame.forEach((player) => {
      const p = new Player(player.playerid, player.color, player.pos);
      this.players.push(p);
    });
  }
}
