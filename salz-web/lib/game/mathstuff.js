export function sketchyMedoid(playerid, frame) {
  // it's not actually the medoid, probably. idk. Will be close enough.
  const playerframe = frame.filter((f) => f.playerid === playerid);

  if (playerframe.length === 0) {
    return [0, 0];
  } else {
    const pframe = playerframe[0];

    const xs = pframe.pos.map((p) => p.x).sort();
    const ys = pframe.pos.map((p) => p.y).sort();

    const xmedian = xs[Math.floor(xs.length / 2)];
    const ymedian = ys[Math.floor(ys.length / 2)];

    return [xmedian, ymedian];
  }
}
