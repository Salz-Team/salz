<script lang="ts">
	let id = '';

	interface Props {
		class?: string;
		ariaLabel: string;
		ondrop?: (event: DragEvent) => void;
		ondragover?: (event: DragEvent) => void;
		ondragexit?: (event: DragEvent) => void;
		ondragenter?: (event: DragEvent) => void;
		ondragleave?: (event: DragEvent) => void;
		children?: import('svelte').Snippet;
	}

	let {
		class: className = '',
		ariaLabel,
		ondrop = () => {},
		ondragover = () => {},
		ondragexit = () => {},
		ondragenter = () => {},
		ondragleave = () => {},
		children,
	}: Props = $props();
</script>

<div
	{id}
	role="region"
	class={'drag-and-drop ' + className}
	{ondrop}
	{ondragover}
	{ondragexit}
	{ondragenter}
	{ondragleave}
	aria-label={ariaLabel}
>
	{@render children?.()}
</div>
