import type { ValuesOf } from '$lib/type-helpers';
import type { TASK_CONTROLS } from '$workers/game-history-parsers/constants';
import type { STATUSES } from '../constants';

export type Request = {
	task: TASK_CONTROLS;
	matchId: string;
	gameHistory: string;
};

/**
 * You are encouraged to use `ResponseTypes` from the same module while passing an `ResponseTypesKey`.
 */
export const ResponseTypes = {
	Status: 'status',
	Error: 'error',
	Completed: 'completed',
} as const;

type ResponseTypesKey = ValuesOf<typeof ResponseTypes>;

export type Response<T = ResponseTypesKey> = {
	type: T;
	status: T extends 'status' ? ResponseStatus : never;
	error: T extends 'error' ? ResponseError : never;
	completed: T extends 'completed' ? ResponseCompleted : never;
};

export type ResponseStatus = {
	type: STATUSES;
	roundsProcessed: number;
	message?: string;
};

export const ErrorCode = {
	AlreadyStarted: 'already_started',
	ParsingError: 'parsing_error',
	NoEnd: 'no_end',
} as const;

/**
 * You are encouraged to use `ErrorCode` from the same module while passing an `ErrorCodeKey`.
 */
type ErrorCodeKey = (typeof ErrorCode)[keyof typeof ErrorCode];

export type ResponseError = {
	code: ErrorCodeKey;
	message: string;
};

export type TicTacToeGame = {
	totalFrames: number;
	endState: {
		isDraw: boolean;
		winner?: number;
		loser?: number;
	};
};

export type ResponseCompleted = {
	gameInfo: TicTacToeGame;
	historyPrefix: string;
};

export const PlaySymbol = {
	X: 'x',
	O: 'o',
} as const;
export type PlaySymbolValue = ValuesOf<typeof PlaySymbol>;

export type PlayerDrawInfo = {
	playerId: number;
	colour: string;
	playerSymbol: PlaySymbolValue;
};

export type BoardDrawingState = {
	state: [[number, number], PlayerDrawInfo][];
	winningLines?: [[number, number], [number, number], [number, number]][];
	errors?: {
		offendingPlayer: number;
		message: string;
	}[];
};
