// @vitest-environment jsdom
import { expect, test } from 'vitest';
import { createSlidingIntInterval } from './sliding-range-window.svelte';

test('createSlidingIntInterval: can slide around a given anchor', () => {
	/* eslint-disable prefer-const */
	let anchor = $state(1);
	let { rangeStart, rangeEnd } = $derived(createSlidingIntInterval(anchor, 5));
	/* eslint-enable prefer-const */

	expect(rangeStart).toEqual(-1);
	expect(rangeEnd).toEqual(3);

	anchor = 7;
	expect(rangeStart).toEqual(5);
	expect(rangeEnd).toEqual(9);
});

test('createSlidingIntInterval: can slide around a given anchor with limits (case: even length)', () => {
	/* eslint-disable prefer-const */
	let anchor = $state(1);
	let { rangeStart, rangeEnd } = $derived(createSlidingIntInterval(anchor, 4, 1, 20));
	/* eslint-enable prefer-const */

	expect(rangeStart).toEqual(1);
	expect(rangeEnd).toEqual(4);

	anchor = 7;
	expect(rangeStart).toEqual(6);
	expect(rangeEnd).toEqual(9);

	anchor = 19;
	expect(rangeStart).toEqual(17);
	expect(rangeEnd).toEqual(20);
});

test('createSlidingIntInterval: can slide around a given anchor with limits (case: odd length)', () => {
	/* eslint-disable prefer-const */
	let anchor = $state(6);
	let { rangeStart, rangeEnd } = $derived(createSlidingIntInterval(anchor, 7, 5, 30));
	/* eslint-enable prefer-const */

	expect(rangeStart).toEqual(5);
	expect(rangeEnd).toEqual(11);

	anchor = 15;
	expect(rangeStart).toEqual(12);
	expect(rangeEnd).toEqual(18);

	anchor = 28;
	expect(rangeStart).toEqual(24);
	expect(rangeEnd).toEqual(30);
});

test('createSlidingIntInterval: returns interval around min or max given an out-of-range anchor', () => {
	/* eslint-disable prefer-const */
	let anchor = $state(1);
	let { rangeStart, rangeEnd } = $derived(createSlidingIntInterval(anchor, 7, 5, 30));
	/* eslint-enable prefer-const */

	expect(rangeStart).toEqual(5);
	expect(rangeEnd).toEqual(11);

	anchor = 35;
	expect(rangeStart).toEqual(24);
	expect(rangeEnd).toEqual(30);
});
