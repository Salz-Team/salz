/**
 * Sets up a debounced updater. To be used with `$derived.by`
 *
 * @example
 * ```typescript
 * let typedText = $state();
 * let searchText = $derived.by(debounce(() => typedText, 300));
 * ```
 */
export function debounce<T>(initialValue: T, getter: () => T, delay: number): () => T {
	let value = $state<T>(initialValue);
	let timer: number;
	$effect(() => {
		const newValue = getter(); // this subscribes to the state
		clearTimeout(timer);
		timer = setTimeout(() => (value = newValue), delay);
		return () => clearTimeout(timer);
	});
	return () => value;
}
