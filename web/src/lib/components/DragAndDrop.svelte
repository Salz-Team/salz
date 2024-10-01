<script lang="ts">
	let id = '';
	let className = '';
	export { className as class };
	export let ariaLabel;

	export let onDrop: (event: DragEvent) => void = () => {};
	export let onDragOver: (event: DragEvent) => void = () => {};
	export let onDragExit: (event: DragEvent) => void = () => {};
	export let onDragEnter: (event: DragEvent) => void = () => {};
	export let onDragLeave: (event: DragEvent) => void = () => {};
</script>

<div
	{id}
	role="region"
	class={'drag-and-drop ' + className}
	on:drop|preventDefault={onDrop}
	on:dragover|preventDefault={onDragOver}
	on:dragexit|preventDefault={onDragExit}
	on:dragenter|preventDefault={onDragEnter}
	on:dragleave|preventDefault={onDragLeave}
	aria-label={ariaLabel}
>
	<slot></slot>
</div>
