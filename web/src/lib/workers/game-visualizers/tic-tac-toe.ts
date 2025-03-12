import localforage from "localforage";
import { TaskControls } from "./constants";
import type { Data } from "./types";
import { PlaySymbol, type BoardDrawingState, type PlaySymbolValue } from "$workers/game-history-parsers/tic-tac-toe.types";
import type { Colourscheme } from "$lib/colorscheme";

let animationFrame: number;
let frame: number = 0;

self.onmessage = async (e: MessageEvent<Data>) => {
  switch (e.data.type) {
    case TaskControls.Init:
      const data = e.data.init;
      if (data.canvas) {
        data.canvas.height = data.canvasHeight;
        data.canvas.width = data.canvasWidth;
        const ctx = data.canvas.getContext('2d');
        if (ctx === null) return;

        animationFrame = requestAnimationFrame(async function loop(t) {
          animationFrame = requestAnimationFrame(loop);
          let curFrameData = await localforage.getItem<BoardDrawingState>(`${data.storePrefix}/${frame}`);
          if (curFrameData) draw(ctx, curFrameData, data.colourscheme, t);
        });
      }
      break;
    case TaskControls.Page:
      frame = e.data.page;
      break;
  }
};

self.onabort = () => {
  cancelAnimationFrame(animationFrame);
};

function draw(
  ctx: OffscreenCanvasRenderingContext2D,
  frameData: BoardDrawingState,
  colourscheme: Colourscheme,
  _: number
) {
  const width = ctx.canvas.width;
  const height = ctx.canvas.height;
  ctx.clearRect(0, 0, width, height);

  // draw tic tac toe borders
  ctx.fillStyle = colourscheme.foreground;
  ctx.fillRect(width / 3.0, 0, 5, height);
  ctx.fillRect(width * 2 / 3.0, 0, 5, height);
  ctx.fillRect(0, height / 3, width, 5);
  ctx.fillRect(0, height * 2 / 3, width, 5);

  for (const playerMove of frameData.state) {
    drawSymbol(ctx, playerMove[0], playerMove[1].playerSymbol, colourscheme, playerMove[1].colour);
  }
}

function drawSymbol(
  ctx: OffscreenCanvasRenderingContext2D,
  coords: [number, number],
  symbol: PlaySymbolValue,
  colourscheme: Colourscheme,
  color: string
) {
  const width = ctx.canvas.width;
  const height = ctx.canvas.height;

  const symbWidth = width / 3 - (2 * width / 10);
  const symbHeight = height / 3 - (2 * height / 10);

  const symbXStart = (width / 3) * (coords[0]) + (width / 10);
  const symbYStart = (height / 3) * (coords[1]) + (height / 10);

  ctx.fillStyle = color;
  ctx.beginPath();
  ctx.lineWidth = 5;
  ctx.strokeStyle = color;
  switch (symbol) {
    case PlaySymbol.X:
      ctx.moveTo(symbXStart, symbYStart);
      ctx.lineTo(symbXStart + symbWidth, symbYStart + symbHeight);
      ctx.stroke();
      ctx.moveTo(symbXStart + symbWidth, symbYStart);
      ctx.lineTo(symbXStart, symbYStart + symbHeight);
      ctx.stroke();
      ctx.closePath();
      ctx.fill();
      break;
    case PlaySymbol.O:
      ctx.arc(symbXStart + (symbWidth / 2), symbYStart + (symbHeight / 2), symbWidth / 2, 0, 2 * Math.PI);
      ctx.strokeStyle = color;
      ctx.stroke();
      ctx.closePath();
      break;
  }
  ctx.lineWidth = 1;
  ctx.strokeStyle = colourscheme.foreground;
  ctx.fillStyle = colourscheme.foreground;
}
