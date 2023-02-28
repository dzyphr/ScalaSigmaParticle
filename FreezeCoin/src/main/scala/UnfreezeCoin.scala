import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}

object UnfreezeCoin {
  def unfreeze(configFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val sender: Address = Address.create(config.getParameters.get("senderAddr"))
    val addrIndex: Int = config.getParameters.get("addrIndex").toInt
    val frozenCoins = Address.create(config.getParameters.get("freezeAddress"))


    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)


      val prover: ErgoProver = ctx.newProverBuilder().withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()


      val senderEIP3: Address = prover.getEip3Addresses().get(0)

      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      //HEADS UP: if you dont take out the full amount it will create a secondary contract which you can withdraw from
      //The secondary address may look like a regular address instead of a script
      val inputboxes = BoxOperations.createForSender(frozenCoins, ctx)
        .withAmountToSpend(ergoAmountFeeSubtracted + Parameters.MinFee )
        .loadTop()
      val signed = BoxOperations.spendBoxesTx(
        ctx,
        ctx.newTxBuilder(),
        inputboxes,
        prover, sender,
        ergoAmountFeeSubtracted,
        Parameters.MinFee
      )
      ctx.sendTransaction(signed)
      signed.toJson(true)

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
    val txJson: String = unfreeze(args(0))
    println(txJson)
  }
}
