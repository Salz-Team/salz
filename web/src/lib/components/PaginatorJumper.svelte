<script lang="ts">
	import Button from './Button.svelte';
	import IconButton from './IconButton.svelte';
	import Tooltip from './Tooltip.svelte';
	import { Position } from './types/Tooltip';

	export let onConfirm: (n: number) => void = (_) => {};
	export let min: number = Number.MIN_VALUE;
	export let max: number = Number.MAX_VALUE;

	let isActive: boolean = false;
	let value: string = '';
	$: confirm = () => {
		isActive = false;
		onConfirm(parseInt(value));
		value = '';
	};
	$: inputKeydownHandler = (
		ev: KeyboardEvent & { currentTarget: EventTarget & HTMLInputElement },
	) => {
		switch (ev.key) {
			case 'Enter':
				isActive = false;
				onConfirm(parseInt(value));
				value = '';
		}
	};
	$: maxlength = String(max).length;
</script>

<div class="paginator-jumper">
	<Button
		onClick={() => {
			isActive = !isActive;
			value = '';
		}}>...</Button
	>
	<Tooltip class="page-jump-input" role="dialog" {isActive} position={Position.TopCenter}>
		<small><label for="page-jump">Jump to page:</label></small>
		<input
			id="page-jump"
			name="page-jump"
			type="number"
			bind:value
			on:keydown={inputKeydownHandler}
			{min}
			{max}
			{maxlength}
      style={`width: ${maxlength + 1}em;`}
		/>
		<IconButton icon="tabler:check" onClick={confirm} />
	</Tooltip>
</div>

<style>
	.paginator-jumper {
		position: relative;
	}

	input[type='number'] {
		width: 3em;
	}

	.paginator-jumper :global(.page-jump-input) {
		width: 12em;
		display: flex;
		justify-content: space-around;
		align-items: center;
	}
</style>
