<script lang="ts">
	import Button from './Button.svelte';
	import IconButton from './IconButton.svelte';
	import Tooltip from './Tooltip.svelte';
	import { Position } from './types/Tooltip';
	import lo from 'lodash';

	interface Props {
		onConfirm?: (n: number) => void;
		min?: number;
		max?: number;
	}

	let { onConfirm = (_) => {}, min = Number.MIN_VALUE, max = Number.MAX_VALUE }: Props = $props();

	let isActive: boolean = $state(false);
	let value: string = $state('');
	let confirm = $derived(() => {
		if (value === null || value === '') return;

		isActive = false;
		const intVal = lo.clamp(parseInt(value), min, max);
		onConfirm(intVal);
		value = '';
	});
	let inputKeydownHandler = $derived(
		(ev: KeyboardEvent & { currentTarget: EventTarget & HTMLInputElement }) => {
			switch (ev.key) {
				case 'Enter':
					ev.preventDefault();
					confirm();
			}
		},
	);
	let maxlength = $derived(String(max).length);
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
			onkeydown={inputKeydownHandler}
			{min}
			{max}
			{maxlength}
			style={`width: ${maxlength + 1}em;`}
		/>
		<IconButton icon="tabler:check" onClick={confirm} isDisabled={value === '' || value === null} />
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
