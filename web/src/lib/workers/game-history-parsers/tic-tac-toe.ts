import { STATUS_IDLE, STATUS_PROCESSING, TASK_START, TASK_STOP } from '$workers/constants';
import type { Request, Response } from './tic-tac-toe.types';

let abortController: AbortController | null = null;

self.onmessage = (e: MessageEvent<Request>) => {
	switch (e.data.task) {
		case TASK_START:
			if (abortController) {
				postMessage({
					type: 'error',
					error: { code: 'already_started', message: 'A process has already started' },
				} as Response<'error'>);
				break;
			}

			abortController = new AbortController();
			postMessage({
				type: 'status',
				status: { type: STATUS_PROCESSING, roundsProcessed: 0 },
			} as Response<'status'>);
			parseHistory(e.data.gameHistory, { signal: abortController.signal });
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

async function parseHistory(s: string, { signal }: { signal: AbortSignal }) {
	const history = s.split('\n');
	if (history.length === 0) return;

	for (let i = 0; i < history.length; i++) {
		console.log(new Date().getTime(), 'signal:', signal.aborted);
		if (signal.aborted) {
			postMessage({
				type: 'status',
				status: { type: STATUS_IDLE, roundsProcessed: 0 },
			} as Response<'status'>);
			break;
		}

		const frame = history[i];
    await sleep(Math.random() * 100);
		postMessage({
			type: 'status',
			status: { type: STATUS_PROCESSING, roundsProcessed: i + 1, message: frame },
		} as Response<'status'>);
	}

  abortController = null;
}

function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
