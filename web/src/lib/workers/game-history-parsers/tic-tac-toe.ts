import { TASK_START, TASK_STOP } from './constants';
import { STATUS_ABORTED, STATUS_IDLE, STATUS_PROCESSING } from '$workers/constants';
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

const PLAYER_APPEARANCE: [PlaySymbolValue, string][] = [
	[PlaySymbol.X, '#e94584'],
	[PlaySymbol.O, '#24aadb'],
];

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
		},
	};
	const lastBoardState: BoardDrawingState = {
		state: [],
		winningLines: undefined,
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
				var playerResponse = frame['responses'][0];
				const playerId = playerResponse['player'];
				if (playerResponse['isValid']) {
					const coords = playerResponse['response']['move'];

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

					let winningLines: [[number, number], [number, number], [number, number]][] = [];

					// check horizontals
					const indexes = [0, 1, 2];
					const horizontallyMatchingRows = indexes
						.map((n) =>
							lastBoardState.state.filter((s: [[number, number], PlayerDrawInfo]) => s[0][1] === n),
						)
						.filter(
							(row: [[number, number], PlayerDrawInfo][]) =>
								row.length === 3 &&
								// make sure that all of the cells are marked by the same player
								row
									.map((c) => c[1])
									.every((cur, idx, arr) => idx === 0 || playerDrawInfoAreEqual(cur, arr[idx - 1])),
						);

					if (horizontallyMatchingRows.length > 0) {
						winningLines.push(...horizontallyMatchingRows.map(boardThreeStatesToCoords));
					}

					const verticallyMatchingRows = indexes
						.map((n) =>
							lastBoardState.state.filter((s: [[number, number], PlayerDrawInfo]) => s[0][0] === n),
						)
						.filter(
							(col: [[number, number], PlayerDrawInfo][]) =>
								col.length === 3 &&
								// make sure that all of the cells are marked by the same player
								col
									.map((c) => c[1])
									.every((cur, idx, arr) => idx === 0 || playerDrawInfoAreEqual(cur, arr[idx - 1])),
						);

					if (verticallyMatchingRows.length > 0) {
						winningLines.push(...verticallyMatchingRows.map(boardThreeStatesToCoords));
					}

					const rightSlantingDiagonal = lastBoardState.state.filter(
						(s: [[number, number], PlayerDrawInfo]) => {
							const c = s[0];
							return (
								(c[0] === 2 && c[1] === 0) ||
								(c[0] === 1 && c[1] === 1) ||
								(c[0] === 0 && c[1] === 2)
							);
						},
					);
					if (
						rightSlantingDiagonal.length === 3 &&
						rightSlantingDiagonal
							.map((c) => c[1])
							.every((cur, idx, arr) => idx === 0 || playerDrawInfoAreEqual(cur, arr[idx - 1]))
					) {
						winningLines.push(boardThreeStatesToCoords(rightSlantingDiagonal));
					}

					const leftSlantingDiagonal = lastBoardState.state.filter(
						(s: [[number, number], PlayerDrawInfo]) => {
							const c = s[0];
							return (
								(c[0] === 0 && c[1] === 0) ||
								(c[0] === 1 && c[1] === 1) ||
								(c[0] === 2 && c[1] === 2)
							);
						},
					);
					if (
						leftSlantingDiagonal.length === 3 &&
						leftSlantingDiagonal
							.map((c) => c[1])
							.every((cur, idx, arr) => idx === 0 || playerDrawInfoAreEqual(cur, arr[idx - 1]))
					) {
						winningLines.push(boardThreeStatesToCoords(leftSlantingDiagonal));
					}

					lastBoardState.winningLines = winningLines.length > 0 ? winningLines : undefined;
				} else {
					lastBoardState.errors = [
						{
							offendingPlayer: playerId,
							message: playerResponse['errorMessage'],
						},
					];
				}

				await localforage.setItem(
					`${STORE_PREFIX}/${matchId}/${ticTacToeGame.totalFrames}`,
					lastBoardState,
				);
				ticTacToeGame.totalFrames++;
				postMessage({
					type: 'status',
					status: { type: STATUS_PROCESSING, roundsProcessed: ticTacToeGame.totalFrames },
				} as Response<'status'>);
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

const boardThreeStatesToCoords = (
	threeStates: [[number, number], PlayerDrawInfo][],
): [[number, number], [number, number], [number, number]] => [
	threeStates[0][0],
	threeStates[1][0],
	threeStates[2][0],
];

const playerDrawInfoAreEqual = (a: PlayerDrawInfo, b: PlayerDrawInfo) =>
	a.playerId === b.playerId && a.playerSymbol === b.playerSymbol;
