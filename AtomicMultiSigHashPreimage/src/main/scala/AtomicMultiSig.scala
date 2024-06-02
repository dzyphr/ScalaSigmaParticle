import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.sdk.SecretString
import sigmastate.Values._


import sigmastate.eval.CostingSigmaDslBuilder.GroupElement
//import sigmastate.interpreter.CryptoConstants._

import java.math.BigInteger



object AtomicMultiSig {
  private def atomicDeposit(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("senderAddr"))
      val addrIndex: Int = config.getParameters.get("addrIndex").toInt
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeIncluded: Long = ergoAmount + Parameters.MinFee
      val preImageHash = "c99aef457c04cf164332153a538f41a7b6f7d942cb9434b5138e17137e15f290"

      val receiver = Address.create(config.getParameters.get("p2Addr")).getPublicKey
      val lockHeight = ctx.getHeight + 10
      val atomicLockScript: String = {
        s"""
            {
                val preimageBYTES = OUTPUTS(0).R4[Coll[Byte]].get;
                sigmaProp(
                    receiver &&
                    sha256(preimageBYTES) == fromBase16(preImageHash) ||
                    sender && (HEIGHT > lockHeight)
                )
            }
        """.stripMargin
      }
      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("sender", sender.getPublicKey)
          .item("receiver", receiver) //receiver is sender just for this example
          .item("preImageHash", preImageHash)
          .item("lockHeight", lockHeight)
          .build(),
        atomicLockScript)
      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()
      val AtomicBox = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outBoxBuilder()
        .value(ergoAmountFeeIncluded)
        .contract(contract)
        .build()
      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(AtomicBox)
        .sendChangeTo(sender.getErgoAddress)
        .build()
      ctx.sendTransaction(prover.sign(unsignedTransaction))

      prover.sign(unsignedTransaction).toJson(true)
    })
    txJson
  }
  private def atomicReceiverClaim(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      def hexStringToByteArray(hex: String): Array[Byte] = {
        val len = hex.length
        val data = new Array[Byte](len / 2)
        var i = 0
        while (i < len) {
          data(i / 2) = ((Character.digit(hex.charAt(i), 16) << 4) +
            Character.digit(hex.charAt(i + 1), 16)).toByte
          i += 2
        }
        data
      }
      val sender: Address = Address.create(config.getParameters.get("p2Addr"))
      val addrIndex: Int = config.getParameters.get("p2AddrIndex").toInt
      val walletMnemonic: String = config.getParameters.get("p2Mnemonic").toString
      val mnemonicPassword: String = config.getParameters.get("p2MnemonicPassword").toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val atomicBox: String = config.getParameters.get("atomicBox").toString
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      val preimage = "1bc295c8bd6b868b5966e8b2079b08529e8d19c95a712dd6de1ae54c16856645"
      val preimageBytes: Array[Byte] = hexStringToByteArray(preimage)

//      val preimagebytes = preimage.getBytes()
      val EVpreimage = ErgoValue.of(preimageBytes)
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(EVpreimage)
        .build()

      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(atomicBox).array.last)

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
  private def atomicSenderReClaim(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("senderAddr"))
      val addrIndex: Int = config.getParameters.get("addrIndex").toInt
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val atomicBox: String = config.getParameters.get("atomicBox").toString

      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted - Parameters.MinFee)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(
          ErgoValue.of(BigInteger.ZERO.toByteArray)
        )
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(atomicBox).array.last)
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
      print("too many args, enter one arg for config file name")
      sys.exit
    }
    else if (args.length == 0) {
      print("enter the config file name as the only argument")
      sys.exit
    }
    //val txJson = atomicDeposit(args(0))
    val txJson = atomicReceiverClaim(args(0))
    //val txJson = atomicSenderReClaim(args(0))
    print(txJson)
  }
}