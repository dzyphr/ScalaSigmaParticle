import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract

object pinUnlock {
  private def pinUnlock(configFileName: String):String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val lockTime: Int = config.getParameters.get("lockTime").toInt
    val sender: Address = Address.create(config.getParameters.get("senderAddr"))
    val addrIndex: Int = config.getParameters.get("addrIndex").toInt


    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val pinLockBoxId: String = config.getParameters.get("pinLockBoxId").toString
      val prover: ErgoProver = ctx.newProverBuilder().withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val senderEIP3: Address = prover.getEip3Addresses().get(addrIndex)
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      val pin = config.getParameters.get("pin").toString
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(ErgoValue.of(pin.getBytes()))
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(pinLockBoxId).array.last)
      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      val signed = prover.sign(tx)

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
    val txJson: String = pinUnlock(args(0))
    println(txJson)
  }
}
