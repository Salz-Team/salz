export const getInitials = (name: string): string =>
	name
		.split(' ')
		.filter((_, i, a) => i == 0 || i == a.length - 1)
		.reduce((acc, cur) => `${acc}${cur.charAt(0)}`, '');
