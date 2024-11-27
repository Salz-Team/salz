<script lang="ts">
	import { page } from '$app/stores';
	import MatchResultList from '$lib/components/MatchResultList.svelte';
	import Paginator from '$lib/components/Paginator.svelte';
	import type { MatchResult } from '$lib/models';
	import { Temporal } from 'temporal-polyfill';

	let currentPage = $state(1);

	let matchId = $derived($page.url.searchParams.get('id'));
	let botResults = $derived([
		{
			id: 'match111',
			timestamp: Temporal.Instant.from('2024-07-19T00:31:19Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar312',
				owner: {
					id: 'foobar1',
					username: 'Foo Bar 1',
				},
			},
			winner: 'bot1',
		},
		{
			id: 'match110',
			timestamp: Temporal.Instant.from('2024-07-19T00:26:07Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar314',
				owner: {
					id: 'foobar2',
					username: 'Foo Bar 2',
				},
			},
			winner: 'bot2',
		},
		{
			id: 'match109',
			timestamp: Temporal.Instant.from('2024-07-17T16:59:07Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar311',
				owner: {
					id: 'foobar3',
					username: 'Foo Bar 3',
				},
			},
			winner: 'bot1',
		},
	] as MatchResult[]);
</script>

{#if matchId === null}
	<h1>Matches</h1>

	<MatchResultList matchResults={botResults} />

	<Paginator class="paginator" totalPages={10} bind:currentPage />
{:else}
	{matchId}<br />

	Show the visualiser here!
{/if}

<style>
	:global(nav.paginator) {
		margin: 1.5em auto;
		width: 80%;
		display: flex;
		justify-content: center;
		align-items: center;
	}
</style>
