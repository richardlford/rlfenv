        /// <summary>
        ///A test for LubOnto
        ///</summary>
        [TestMethod()]
        [DeploymentItem("Bartok.Ir.dll")]
        public void LubOntoTestL1[[label2]]() {
            v_actual.Set(v_local1);
            v_expected.Set(?2);
            bool expected = ?3;
            bool actual = v_actual.LubOnto([[rhs]]);
            Assert.AreEqual(expected, actual);
            Assert.IsTrue(v_expected.Equals(v_actual));
        }

