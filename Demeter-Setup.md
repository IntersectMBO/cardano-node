# Setting Up a Cardano Node Using Demeter

This guide provides a clear and professional walkthrough on how to deploy and manage a Cardano node using **Demeter**, an infrastructure-as-a-service platform that simplifies Cardano development by providing ready-to-use environments.

---

## ## Prerequisites

Before beginning, ensure you have:

* A Demeter account ([https://demeter.run](https://demeter.run))
* Basic understanding of Cardano architecture (nodes, relays, chain sync)
* Familiarity with web dashboards and developer tooling

---

## 1. Create a New Environment on Demeter

1. Log in to your Demeter dashboard.
2. Click **Create Environment**.
3. Choose a workspace name and description.
4. Select a subscription tier appropriate for your node requirements.
5. Proceed to the template section.

---

## 2. Select the Cardano Node Template

Demeter provides pre-configured Cardano infrastructure templates. To deploy a node:

1. Navigate to **Templates**.
2. Select **Cardano Node** (Full Node) or **Cardano Relay**, depending on your purpose.
3. Click **Use Template**.
4. Review configuration options such as:

   * Node type: Full Node / Relay
   * Network: Mainnet / Preprod / Preview
   * Storage and memory allocation
5. Confirm your selection.

---

## 3. Configure Node Parameters

You can customize your node configuration before deployment.

Typical configuration options include:

* **Port settings** (for p2p and local client access)
* **P2P topology** (incoming/outgoing peers, min/max peers)
* **Tracing settings** for logging
* **Kupo or Ogmios integration options** if selected

Adjust values as needed for your use case, or keep defaults for a standard setup.

---

## 4. Deploy the Node

Once configuration is complete:

1. Click **Deploy Environment**.
2. Demeter will automatically provision:

   * Compute resources
   * Storage volumes
   * Networking setup
   * Cardano node binaries
3. The deployment process usually completes within a few minutes.

After deployment, you can monitor:

* Node sync progress
* CPU / RAM usage
* Logs and health status

---

## 5. Accessing Your Cardano Node

Demeter provides multiple access options.

### **5.1 Access via Terminal (Web Console)**

1. Open your environment dashboard.
2. Click **Terminal**.
3. Run common node commands, such as:

   ```bash
   cardano-cli query tip --mainnet
   ```

### **5.2 Connect Externally via API Services**

Depending on your template, you may have:

* **Ogmios** endpoint
* **Kupo** endpoint
* **Blockfrost-style API**

These endpoints will be visible under the **Services** panel.

---

## 6. Checking Node Sync Status

To confirm your node is syncing properly, run:

```bash
cardano-cli query tip --mainnet
```

Compare the output with a live explorer (e.g., AdaStat, PoolTool) to verify sync height.

Demeter’s dashboard also shows real-time sync progress.

---

## 7. Managing and Updating the Node

Demeter handles most infrastructure updates automatically.

### **Updating Node Version**

In your environment settings:

1. Navigate to **Node Configuration**.
2. Select a newer Cardano node version if available.
3. Apply changes and restart.

### **Scaling Resources**

You can increase memory or storage at any time by editing your environment.

---

## 8. Stopping or Destroying the Environment

* **Stop Environment** temporarily pauses compute usage (saves cost).
* **Destroy Environment** removes the node and associated resources.

Both options are available in the environment dashboard.

---

## 9. Best Practices

* Use P2P mode for improved connectivity.
* Regularly monitor logs for unexpected behavior.
* Back up important configuration files if modified.
* Avoid exposing ports publicly unless necessary.

---

## 10. Additional Resources

* Demeter Documentation
* Cardano Developer Portal
* Cardano Node GitHub Repository

---

## Conclusion

Using Demeter significantly simplifies the complexity of deploying and managing Cardano nodes. With ready-to-use templates, automatic infrastructure provisioning, and convenient monitoring tools, developers can focus on building instead of maintaining low-level configurations.

This markdown file can be added directly to any Cardano open-source repository as part of your contribution.
