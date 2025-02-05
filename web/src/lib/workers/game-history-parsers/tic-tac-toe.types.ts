import type { STATUSES, TASK_CONTROLS } from '$workers/constants';

export type Request = {
	task: TASK_CONTROLS;
	gameHistory: string;
};

export type ResponseTypes = 'status' | 'error';

export type Response<T = ResponseTypes> = {
  type: T;
  status: T extends 'status' ? ResponseStatus : never;
  error: T extends 'error' ? ResponseError : never;
};

export type ResponseStatus = {
	type: STATUSES;
	roundsProcessed: number;
  message?: string;
};

export type ErrorCode = 'already_started';

export type ResponseError = {
  code: ErrorCode;
  message: string;
};
