<script lang="ts">
	import Icon from '@iconify/svelte';
	import DragAndDrop from '$lib/components/DragAndDrop.svelte';
	import Button from '$lib/components/Button.svelte';

	let dragState = $state('');

	let fileInput = $state<HTMLInputElement>();
	let file: File | null = $state(null);

	function handleDrop(event: DragEvent) {
		event.preventDefault();
		dragState = 'dropped';
		console.log(event.dataTransfer?.files);
		file = event.dataTransfer?.files.item(0) ?? null;
		dragState = '';
	}

	function handleDragOver(event: DragEvent) {
		event.preventDefault();
		dragState = 'dragover';
	}

	function handleDragExit(event: DragEvent) {
		event.preventDefault();
		dragState = '';
	}

	function resetForm() {
		file = null;
		dragState = '';
	}
</script>

<main>
	<form action="#">
		<DragAndDrop
			class={dragState}
			ondrop={handleDrop}
			ondragover={handleDragOver}
			ondragexit={handleDragExit}
			ariaLabel="Drag and drop a file into this area"
		>
			<input
				class="bot-file-button"
				type="file"
				id="botFile"
				name="botFile"
				bind:this={fileInput}
			/>
			<label for="botFile">
				<span>
					<Icon icon="tabler:cloud-upload" /> Upload Bot File
				</span><br />
				Click or Drag and drop files here<br />
			</label>
		</DragAndDrop>
		{file?.name ?? ''}<br />
		<Button type="button" isDisabled={!file?.name}>Upload</Button>
		<Button type="button" onClick={resetForm} isDisabled={!file?.name}>Reset</Button>
	</form>
</main>

<style>
	main {
		width: 60vw;
		min-width: 600px;
		padding: 2em 2em;
		margin: 0 auto;
	}

	main :global(.drag-and-drop) {
		border: 1px solid black;
		height: 300px;
		text-align: center;
		transition: background 0.5s ease-in-out;
		position: relative;
	}

	main :global(.drag-and-drop.dragover) {
		background: lightblue;
	}

	label[for='botFile'] {
		display: flex;
		flex-direction: column;
		width: 100%;
		height: 100%;
		justify-content: center;
		align-items: center;
	}

	.bot-file-button {
		opacity: 0;
		position: absolute;
	}
</style>
