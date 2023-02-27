import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}



object FreezeCoinObject {
  private def freezeCoin(configFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val lockTime: Int = config.getParameters.get("lockTime").toInt
    val sender: Address = Address.create(config.getParameters.get("senderAddr"))
    val addrIndex: Int = config.getParameters.get("addrIndex").toInt
    println(sender.asP2PK())

    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)



      val prover: ErgoProver = ctx.newProverBuilder().withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      println(prover.getEip3Addresses.get(0))

      val senderEIP3: Address = prover.getEip3Addresses().get(0)
      println(senderEIP3.toString)
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeIncluded: Long = ergoAmount + Parameters.MinFee

      val freezeCoinScript: String =
        s"""
                { sigmaProp(HEIGHT > freezeDeadline) && sender }
         """.stripMargin

      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("freezeDeadline", ctx.getHeight + lockTime)
          .item("sender", sender.getPublicKey)
          .build(),
        freezeCoinScript)

      //TODO GET INPUT BOXES LOADED
      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()

      val freezeBox = ctx.newTxBuilder()
          .boxesToSpend(inputboxes)
          .outBoxBuilder()
          .value(ergoAmountFeeIncluded)
          .contract(contract).build()

      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(freezeBox)
        .sendChangeTo(senderEIP3.getErgoAddress)
        .build()

      val signedContractTx: SignedTransaction = prover.sign(unsignedTransaction)

      /*val txId =*/ctx.sendTransaction(signedContractTx)

      signedContractTx.toJson(true)
    })
    txJson
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("too many args, enter one arg for config file name")
      sys.exit
    }
    else if (args.length == 0) {
      print("enter the config file name as the only argument")
      sys.exit
    }
    val txJson: String = freezeCoin(args(0))
    println(txJson)
  }
}
