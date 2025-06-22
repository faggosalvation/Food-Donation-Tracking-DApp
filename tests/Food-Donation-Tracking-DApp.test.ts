const { assertEquals, types } = require('@stacks/assertions');
const { Client, Provider, ProviderRegistry } = require('@stacks/rpc-client');

describe('Food Donation Tracking Tests', () => {
  let client;
  let provider;

  before(async () => {
    provider = await ProviderRegistry.createProvider();
    client = new Client({ provider });
  });

  it('should create donation successfully', async () => {
    const amount = 100;
    const recipient = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM';

    const result = await client.createDonation(recipient, amount);
    assertEquals(result.success, true);
  });

  it('should verify donation successfully', async () => {
    const donationId = 1;
    const result = await client.verifyDonation(donationId);
    assertEquals(result.success, true);
  });

  it('should fail with invalid amount', async () => {
    const amount = 0;
    const recipient = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM';

    const result = await client.createDonation(recipient, amount);
    assertEquals(result.error, 'ERR-INVALID-AMOUNT');
  });
});
