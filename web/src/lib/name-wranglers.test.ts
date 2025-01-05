import { describe, expect, it } from 'vitest';
import { getInitials } from './name-wranglers';

describe('getInitials', () => {
	it("converts 'JohnSmith' to 'J'", () => expect(getInitials('JohnSmith')).toBe('J'));
	it("converts 'John Smith' to 'JS'", () => expect(getInitials('John Smith')).toBe('JS'));
	it("converts 'john' to 'j'", () => expect(getInitials('john')).toBe('j'));
	it("converts '风生水起' to '风'", () => expect(getInitials('风生水起')).toBe('风'));
	it('only returns at most 2 characters, with the first and last part of the name', () =>
		expect(getInitials('John Smith Third')).toBe('JT'));
	it("doesn't handle emojis well (which is fine)", () => expect(getInitials('😻')).not.toBe('😻'));
});
