import localforage from 'localforage';
import { TaskControls } from './constants';
import type { Data } from './types';
import {
	PlaySymbol,
	type BoardDrawingState,
	type PlaySymbolValue,
} from '$workers/game-history-parsers/tic-tac-toe.types';
import type { Colourscheme } from '$lib/colorscheme';
import { ColorChoice } from '$lib/components/types/CheckeredPattern';

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
					let curFrameData = await localforage.getItem<BoardDrawingState>(
						`${data.storePrefix}/${frame}`,
					);
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
	_: number,
) {
	const width = ctx.canvas.width;
	const height = ctx.canvas.height;
	ctx.clearRect(0, 0, width, height);

	// draw tic tac toe borders
	ctx.fillStyle = colourscheme.foreground;
	ctx.fillRect(width / 3.0, 0, 5, height);
	ctx.fillRect((width * 2) / 3.0, 0, 5, height);
	ctx.fillRect(0, height / 3, width, 5);
	ctx.fillRect(0, (height * 2) / 3, width, 5);

	for (const playerMove of frameData.state) {
		drawSymbol(ctx, playerMove[0], playerMove[1].playerSymbol, colourscheme, playerMove[1].colour);
	}

	if (frameData.winningLines) {
		const getX = (xCoord: number): number => (xCoord === 0 ? 0 : xCoord === 2 ? width : width / 2);
		const getY = (yCoord: number): number =>
			yCoord === 0 ? 0 : yCoord === 2 ? height : height / 2;

		ctx.fillStyle = colourscheme.brightGreen;
		ctx.lineWidth = 5;
		ctx.strokeStyle = colourscheme.brightGreen;
		for (const winningLine of frameData.winningLines) {
			ctx.beginPath();
			ctx.moveTo(getX(winningLine[0][0]), getY(winningLine[0][1]));
			for (const coords of winningLine) {
				ctx.lineTo(getX(coords[0]), getY(coords[1]));
				ctx.stroke();
				ctx.closePath();
				ctx.fill();
			}
		}
	}
}

function drawSymbol(
	ctx: OffscreenCanvasRenderingContext2D,
	coords: [number, number],
	symbol: PlaySymbolValue,
	colourscheme: Colourscheme,
	color: string,
) {
	const width = ctx.canvas.width;
	const height = ctx.canvas.height;

	const symbWidth = width / 3 - (2 * width) / 10;
	const symbHeight = height / 3 - (2 * height) / 10;

	const symbXStart = (width / 3) * coords[0] + width / 10;
	const symbYStart = (height / 3) * coords[1] + height / 10;

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
			ctx.arc(
				symbXStart + symbWidth / 2,
				symbYStart + symbHeight / 2,
				symbWidth / 2,
				0,
				2 * Math.PI,
			);
			ctx.strokeStyle = color;
			ctx.stroke();
			ctx.closePath();
			break;
	}
	ctx.lineWidth = 1;
	ctx.strokeStyle = colourscheme.foreground;
	ctx.fillStyle = colourscheme.foreground;
}
