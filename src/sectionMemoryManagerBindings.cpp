#include "sectionMemoryManagerBindings.hpp"

extern "C" {

    llvm::SectionMemoryManager* bnLLVMCreateSectionMemoryManager() {
        return new llvm::SectionMemoryManager();
    }

    uint8_t* bnLLVMSectionManagerAllocateCodeSection(
        llvm::SectionMemoryManager *memoryManager,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        llvm::StringRef SectionName
    ) {
        return memoryManager->allocateCodeSection(
            Size, Alignment,
            SectionID, SectionName
        );
    }

    uint8_t* bnLLVMSectionManagerAllocateDataSection(
            llvm::SectionMemoryManager *memoryManager,
            uintptr_t Size, unsigned Alignment,
            unsigned SectionID, llvm::StringRef SectionName,
            bool isReadOnly
    ) {
        return memoryManager->allocateDataSection(
            Size, Alignment,
            SectionID, SectionName,
            isReadOnly
        );
    }

    bool bnLLVMSectionManagerFinalizeMemory(
        llvm::SectionMemoryManager *memoryManager,
        std::string *ErrMsg
    ) {
        return memoryManager->finalizeMemory(ErrMsg);
    }

    void bnLLVMDestroySectionMemoryManager(llvm::SectionMemoryManager* memoryManager) {
        delete memoryManager;
    }
}