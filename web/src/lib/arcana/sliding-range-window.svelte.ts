import lo from 'lodash';

export type RangeSlider = {
	leftOffset: number;
	rightOffset: number;
	rangeStart: number;
	rangeEnd: number;
};

export function createSlidingIntInterval(
	anchor: number,
	intervalLength: number,
	min: number = Number.MIN_SAFE_INTEGER,
	max: number = Number.MAX_SAFE_INTEGER,
): RangeSlider {
	const [leftOffset, rightOffset]: [number, number] =
		intervalLength % 2 === 0
			? [intervalLength / 2 - 1, intervalLength / 2]
			: (new Array(2).fill((intervalLength - 1) / 2) as [number, number]);

	// Calling these pragmas to ignore the prefer-const lint.
	// This is because using `const` would break reactivity.
	// eslint-plugin-svelte for Svelte 5 is still experiemental.
	/* eslint-disable prefer-const */
	let a = $derived(lo.clamp(anchor, min, max));
	let rangeStart = $derived(lo.clamp(a - leftOffset, min, max - intervalLength + 1));
	let rangeEnd = $derived(
		rangeStart === min
			? min - 1 + intervalLength
			: rangeStart + intervalLength > max
				? max
				: Math.min(a + rightOffset, max - 1),
	);
	/* eslint-enable prefer-const */

	return {
		leftOffset,
		rightOffset,
		rangeStart,
		rangeEnd,
	};
}
