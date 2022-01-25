#include <llvm/ExecutionEngine/SectionMemoryManager.h>

extern "C" {
    llvm::SectionMemoryManager* bnLLVMCreateSectionMemoryManager();
    uint8_t* bnLLVMSectionManagerAllocateCodeSection(
        llvm::SectionMemoryManager *memoryManager,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        llvm::StringRef SectionName
    );

    uint8_t* bnLLVMSectionManagerAllocateDataSection(
            llvm::SectionMemoryManager *memoryManager,
            uintptr_t Size, unsigned Alignment,
            unsigned SectionID, llvm::StringRef SectionName,
            bool isReadOnly
    );

    bool bnLLVMSectionManagerFinalizeMemory(
        llvm::SectionMemoryManager *memoryManager,
        std::string *ErrMsg
    );

    void bnLLVMDestroySectionMemoryManager(llvm::SectionMemoryManager* memoryManager);
}