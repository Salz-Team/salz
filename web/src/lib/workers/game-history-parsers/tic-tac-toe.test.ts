import '@vitest/web-worker';
import TicTacToeHistoryParser from './tic-tac-toe.ts?worker';
import { describe, expect, it } from 'vitest';

describe('tic-tac-toe worker', () => {
	const worker = new TicTacToeHistoryParser();

	it('echos', () => {
		worker.postMessage({ gameHistory: 'hi' });
		worker.onmessage = (e) => {
			expect(e.data.message).toBe('hi');
		};
	});
});
