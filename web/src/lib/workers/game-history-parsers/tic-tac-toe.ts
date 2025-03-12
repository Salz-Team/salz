import {
	TASK_START,
	TASK_STOP,
} from './constants';
import {
	STATUS_ABORTED,
	STATUS_IDLE,
	STATUS_PROCESSING
} from "$workers/constants";
import localforage from 'localforage';
import {
	ErrorCode,
	type BoardDrawingState,
	type PlayerDrawInfo,
	type Request,
	type Response,
	type TicTacToeGame,
	type PlaySymbolValue,
	PlaySymbol,
} from './tic-tac-toe.types';
import { STORE_PREFIX } from './tic-tac-toe.constants';
import _ from 'lodash';

const PLAYER_APPEARANCE: [PlaySymbolValue, string][] = [[PlaySymbol.X, "#e94584"], [PlaySymbol.O, "#24aadb"]];

let abortController: AbortController | null = null;

self.onmessage = (e: MessageEvent<Request>) => {
	switch (e.data.task) {
		case TASK_START:
			if (abortController) {
				postMessage({
					type: 'error',
					error: { code: ErrorCode.AlreadyStarted, message: 'A process has already started' },
				} as Response<'error'>);
				break;
			}

			abortController = new AbortController();
			parseHistory(e.data.gameHistory, e.data.matchId, { signal: abortController.signal });
			break;
		case TASK_STOP:
			if (abortController) {
				abortController.abort();
			}
			break;
		default:
			postMessage({ status: 'finished', message: e.data.gameHistory });
	}
};

self.onabort = () => {
	abortController = null;
};

async function parseHistory(s: string, matchId: string, { signal }: { signal: AbortSignal }) {
	postMessage({
		type: 'status',
		status: { type: STATUS_PROCESSING, roundsProcessed: 0 },
	} as Response<'status'>);

	let hasError = false;
	let hasEnded = false;
	const ticTacToeGame: TicTacToeGame = {
		totalFrames: 0,
		endState: {
			isDraw: true,
		}
	};
	const lastBoardState: BoardDrawingState = {
		state: [],
		winningLine: undefined,
		errors: undefined,
	};
	const playerAppearances: Record<number, [PlaySymbolValue, string]> = {};
	const playerAppearanceCopy = _.cloneDeep(PLAYER_APPEARANCE);

	const history = s.split('\n');
	if (history.length === 0) return;

	for (let i = 0; i < history.length; i++) {
		if (signal.aborted) {
			postMessage({
				type: 'status',
				status: { type: STATUS_ABORTED, roundsProcessed: i },
			} as Response<'status'>);
			break;
		}

		const frame = JSON.parse(history[i]);
		if (!Object.hasOwn(frame, 'messageType')) {
			hasError = true;
			postMessage({
				type: 'error',
				error: {
					code: ErrorCode.ParsingError,
					message: `Could not parse line ${i + 1}. Does not have "messageType".`,
				},
			} as Response<'error'>);
			break;
		}

		switch (frame['messageType']) {
			case 'gameStart':
				// do nothing
				break;
			case 'playerResponses':
				var playerResponse = frame["responses"][0];
				const playerId = playerResponse["player"];
				if (playerResponse["isValid"]) {
					const coords = playerResponse["response"]["move"];

					if (!Object.hasOwn(playerAppearances, playerId)) {
						playerAppearances[playerId] = playerAppearanceCopy.pop()!;
					}

					const playerDrawInfo: PlayerDrawInfo = {
						playerId,
						playerSymbol: playerAppearances[playerId][0],
						colour: playerAppearances[playerId][1],
					};
					lastBoardState.state.push([coords, playerDrawInfo]);
					lastBoardState.errors = undefined;

					let winningLine: [[number, number], [number, number], [number, number]] | null = null;
				} else {
					lastBoardState.errors = [
						{
							offendingPlayer: playerId,
							message: playerResponse["errorMessage"]
						}
					];
				}

				await localforage.setItem(`${STORE_PREFIX}/${matchId}/${ticTacToeGame.totalFrames}`, lastBoardState);
				ticTacToeGame.totalFrames++;
				break;
			case 'gameEnd':
				hasEnded = true;
				const winner = (frame?.['scores'] as Record<string, number>[]).find(
					(p) => p?.['score'] == 1,
				)?.['player'];
				const losers = (frame?.['scores'] as Record<string, number>[])
					.filter((p) => p?.['score'] == 0)
					.map((p) => p?.['player']);
				if (losers.length === 2) {
					ticTacToeGame.endState.isDraw = true;
				} else if (winner === undefined) {
					hasError = true;
					postMessage({
						type: 'error',
						error: {
							code: ErrorCode.ParsingError,
							message: `Could not parse line ${i + 1} (gameEnd message). Invalid win-lose state.`,
						},
					} as Response<'error'>);
				} else {
					ticTacToeGame.endState = { isDraw: false, winner, loser: losers[0] };
				}
				break;
		}

		await sleep(Math.random() * 100);
		postMessage({
			type: 'status',
			status: { type: STATUS_PROCESSING, roundsProcessed: i + 1 },
		} as Response<'status'>);
	}

	if (signal.aborted || hasError || !hasEnded) {
		await Promise.all(
			(await localforage.keys())
				.filter((k) => k.startsWith(`${STORE_PREFIX}/${matchId}`))
				.map((k) => localforage.removeItem(k)),
		);
	}

	if (!signal.aborted && !hasEnded) {
		postMessage({
			type: 'error',
			error: { code: ErrorCode.NoEnd, message: 'Game history file does not have an end log' },
		} as Response<'error'>);
	} else {
		postMessage({
			type: 'completed',
			completed: { gameInfo: ticTacToeGame, historyPrefix: `${STORE_PREFIX}/${matchId}` },
		} as Response<'completed'>);
	}

	postMessage({
		type: 'status',
		status: { type: STATUS_IDLE, roundsProcessed: 0 },
	} as Response<'status'>);

	abortController = null;
}

function sleep(ms: number) {
	return new Promise((resolve) => setTimeout(resolve, ms));
}
